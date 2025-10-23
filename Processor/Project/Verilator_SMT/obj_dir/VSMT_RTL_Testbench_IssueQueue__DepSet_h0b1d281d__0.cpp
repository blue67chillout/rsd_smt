// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_IssueQueue.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__0(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellinp__issueQueueFreeList__rst = 
        ((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) 
         | (IData)(vlSelfRef.__PVT__freeListReset));
    vlSelfRef.__Vcellinp__intPayloadRAM__we[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [0U];
    vlSelfRef.__Vcellinp__intPayloadRAM__we[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [1U];
    vlSelfRef.__Vcellinp__complexPayloadRAM__we[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [0U];
    vlSelfRef.__Vcellinp__complexPayloadRAM__we[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [1U];
    vlSelfRef.__Vcellinp__memPayloadRAM__we[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [0U];
    vlSelfRef.__Vcellinp__memPayloadRAM__we[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [1U];
    vlSelfRef.__Vcellinp__fpPayloadRAM__we[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [0U];
    vlSelfRef.__Vcellinp__fpPayloadRAM__we[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__we[0U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__we[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__we[1U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__we[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__we[0U] 
        = vlSelfRef.__Vcellinp__complexPayloadRAM__we
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__we[1U] 
        = vlSelfRef.__Vcellinp__complexPayloadRAM__we
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__we[0U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__we[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__we[1U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__we[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__we[0U] 
        = vlSelfRef.__Vcellinp__fpPayloadRAM__we[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__we[1U] 
        = vlSelfRef.__Vcellinp__fpPayloadRAM__we[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__1(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellout__intPayloadRAM__rv[0U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__rv
        [0U][0U];
    vlSelfRef.__Vcellout__intPayloadRAM__rv[0U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__rv
        [0U][1U];
    vlSelfRef.__Vcellout__intPayloadRAM__rv[0U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__rv
        [0U][2U];
    vlSelfRef.__Vcellout__intPayloadRAM__rv[0U][3U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__rv
        [0U][3U];
    vlSelfRef.__Vcellout__intPayloadRAM__rv[0U][4U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__rv
        [0U][4U];
    vlSelfRef.__Vcellout__intPayloadRAM__rv[1U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__rv
        [1U][0U];
    vlSelfRef.__Vcellout__intPayloadRAM__rv[1U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__rv
        [1U][1U];
    vlSelfRef.__Vcellout__intPayloadRAM__rv[1U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__rv
        [1U][2U];
    vlSelfRef.__Vcellout__intPayloadRAM__rv[1U][3U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__rv
        [1U][3U];
    vlSelfRef.__Vcellout__intPayloadRAM__rv[1U][4U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__rv
        [1U][4U];
    vlSelfRef.__PVT__intIssuedData[0U][0U] = vlSelfRef.__Vcellout__intPayloadRAM__rv
        [0U][0U];
    vlSelfRef.__PVT__intIssuedData[0U][1U] = vlSelfRef.__Vcellout__intPayloadRAM__rv
        [0U][1U];
    vlSelfRef.__PVT__intIssuedData[0U][2U] = vlSelfRef.__Vcellout__intPayloadRAM__rv
        [0U][2U];
    vlSelfRef.__PVT__intIssuedData[0U][3U] = vlSelfRef.__Vcellout__intPayloadRAM__rv
        [0U][3U];
    vlSelfRef.__PVT__intIssuedData[0U][4U] = vlSelfRef.__Vcellout__intPayloadRAM__rv
        [0U][4U];
    vlSelfRef.__PVT__intIssuedData[1U][0U] = vlSelfRef.__Vcellout__intPayloadRAM__rv
        [1U][0U];
    vlSelfRef.__PVT__intIssuedData[1U][1U] = vlSelfRef.__Vcellout__intPayloadRAM__rv
        [1U][1U];
    vlSelfRef.__PVT__intIssuedData[1U][2U] = vlSelfRef.__Vcellout__intPayloadRAM__rv
        [1U][2U];
    vlSelfRef.__PVT__intIssuedData[1U][3U] = vlSelfRef.__Vcellout__intPayloadRAM__rv
        [1U][3U];
    vlSelfRef.__PVT__intIssuedData[1U][4U] = vlSelfRef.__Vcellout__intPayloadRAM__rv
        [1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData[0U][0U] 
        = vlSelfRef.__PVT__intIssuedData[0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData[0U][1U] 
        = vlSelfRef.__PVT__intIssuedData[0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData[0U][2U] 
        = vlSelfRef.__PVT__intIssuedData[0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData[0U][3U] 
        = vlSelfRef.__PVT__intIssuedData[0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData[0U][4U] 
        = vlSelfRef.__PVT__intIssuedData[0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData[1U][0U] 
        = vlSelfRef.__PVT__intIssuedData[1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData[1U][1U] 
        = vlSelfRef.__PVT__intIssuedData[1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData[1U][2U] 
        = vlSelfRef.__PVT__intIssuedData[1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData[1U][3U] 
        = vlSelfRef.__PVT__intIssuedData[1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData[1U][4U] 
        = vlSelfRef.__PVT__intIssuedData[1U][4U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__2(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellout__memPayloadRAM__rv[0U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__rv
        [0U][0U];
    vlSelfRef.__Vcellout__memPayloadRAM__rv[0U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__rv
        [0U][1U];
    vlSelfRef.__Vcellout__memPayloadRAM__rv[0U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__rv
        [0U][2U];
    vlSelfRef.__Vcellout__memPayloadRAM__rv[0U][3U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__rv
        [0U][3U];
    vlSelfRef.__Vcellout__memPayloadRAM__rv[1U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__rv
        [1U][0U];
    vlSelfRef.__Vcellout__memPayloadRAM__rv[1U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__rv
        [1U][1U];
    vlSelfRef.__Vcellout__memPayloadRAM__rv[1U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__rv
        [1U][2U];
    vlSelfRef.__Vcellout__memPayloadRAM__rv[1U][3U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__rv
        [1U][3U];
    vlSelfRef.__PVT__memIssuedData[0U][0U] = vlSelfRef.__Vcellout__memPayloadRAM__rv
        [0U][0U];
    vlSelfRef.__PVT__memIssuedData[0U][1U] = vlSelfRef.__Vcellout__memPayloadRAM__rv
        [0U][1U];
    vlSelfRef.__PVT__memIssuedData[0U][2U] = vlSelfRef.__Vcellout__memPayloadRAM__rv
        [0U][2U];
    vlSelfRef.__PVT__memIssuedData[0U][3U] = vlSelfRef.__Vcellout__memPayloadRAM__rv
        [0U][3U];
    vlSelfRef.__PVT__memIssuedData[1U][0U] = vlSelfRef.__Vcellout__memPayloadRAM__rv
        [1U][0U];
    vlSelfRef.__PVT__memIssuedData[1U][1U] = vlSelfRef.__Vcellout__memPayloadRAM__rv
        [1U][1U];
    vlSelfRef.__PVT__memIssuedData[1U][2U] = vlSelfRef.__Vcellout__memPayloadRAM__rv
        [1U][2U];
    vlSelfRef.__PVT__memIssuedData[1U][3U] = vlSelfRef.__Vcellout__memPayloadRAM__rv
        [1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData[0U][0U] 
        = vlSelfRef.__PVT__memIssuedData[0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData[0U][1U] 
        = vlSelfRef.__PVT__memIssuedData[0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData[0U][2U] 
        = vlSelfRef.__PVT__memIssuedData[0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData[0U][3U] 
        = vlSelfRef.__PVT__memIssuedData[0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData[1U][0U] 
        = vlSelfRef.__PVT__memIssuedData[1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData[1U][1U] 
        = vlSelfRef.__PVT__memIssuedData[1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData[1U][2U] 
        = vlSelfRef.__PVT__memIssuedData[1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData[1U][3U] 
        = vlSelfRef.__PVT__memIssuedData[1U][3U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__3(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlWide<3>/*81:0*/ __Vlvbound_h3ac9a64e__0;
    VL_ZERO_W(82, __Vlvbound_h3ac9a64e__0);
    // Body
    vlSelfRef.__Vcellout__complexPayloadRAM__rv[0U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__rv
        [0U][0U];
    vlSelfRef.__Vcellout__complexPayloadRAM__rv[0U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__rv
        [0U][1U];
    vlSelfRef.__Vcellout__complexPayloadRAM__rv[0U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__rv
        [0U][2U];
    vlSelfRef.__PVT__complexIssuedData[0U][0U] = vlSelfRef.__Vcellout__complexPayloadRAM__rv
        [0U][0U];
    vlSelfRef.__PVT__complexIssuedData[0U][1U] = vlSelfRef.__Vcellout__complexPayloadRAM__rv
        [0U][1U];
    vlSelfRef.__PVT__complexIssuedData[0U][2U] = vlSelfRef.__Vcellout__complexPayloadRAM__rv
        [0U][2U];
    __Vlvbound_h3ac9a64e__0[0U] = vlSelfRef.__PVT__complexIssuedData
        [0U][0U];
    __Vlvbound_h3ac9a64e__0[1U] = vlSelfRef.__PVT__complexIssuedData
        [0U][1U];
    __Vlvbound_h3ac9a64e__0[2U] = vlSelfRef.__PVT__complexIssuedData
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuedData[0U][0U] 
        = __Vlvbound_h3ac9a64e__0[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuedData[0U][1U] 
        = __Vlvbound_h3ac9a64e__0[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuedData[0U][2U] 
        = __Vlvbound_h3ac9a64e__0[2U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__4(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlWide<3>/*92:0*/ __Vlvbound_h5840327a__0;
    VL_ZERO_W(93, __Vlvbound_h5840327a__0);
    // Body
    vlSelfRef.__Vcellout__fpPayloadRAM__rv[0U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__rv
        [0U][0U];
    vlSelfRef.__Vcellout__fpPayloadRAM__rv[0U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__rv
        [0U][1U];
    vlSelfRef.__Vcellout__fpPayloadRAM__rv[0U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__rv
        [0U][2U];
    vlSelfRef.__PVT__fpIssuedData[0U][0U] = vlSelfRef.__Vcellout__fpPayloadRAM__rv
        [0U][0U];
    vlSelfRef.__PVT__fpIssuedData[0U][1U] = vlSelfRef.__Vcellout__fpPayloadRAM__rv
        [0U][1U];
    vlSelfRef.__PVT__fpIssuedData[0U][2U] = vlSelfRef.__Vcellout__fpPayloadRAM__rv
        [0U][2U];
    __Vlvbound_h5840327a__0[0U] = vlSelfRef.__PVT__fpIssuedData
        [0U][0U];
    __Vlvbound_h5840327a__0[1U] = vlSelfRef.__PVT__fpIssuedData
        [0U][1U];
    __Vlvbound_h5840327a__0[2U] = vlSelfRef.__PVT__fpIssuedData
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuedData[0U][0U] 
        = __Vlvbound_h5840327a__0[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuedData[0U][1U] 
        = __Vlvbound_h5840327a__0[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuedData[0U][2U] 
        = __Vlvbound_h5840327a__0[2U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__5(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__5\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellout__issueQueueFreeList__poppedData[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__poppedData
        [0U];
    vlSelfRef.__Vcellout__issueQueueFreeList__poppedData[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__poppedData
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocatedPtr[0U] 
        = vlSelfRef.__Vcellout__issueQueueFreeList__poppedData
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocatedPtr[1U] 
        = vlSelfRef.__Vcellout__issueQueueFreeList__poppedData
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__0(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellinp__issueQueueFreeList__pop[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocate
        [0U];
    vlSelfRef.__Vcellinp__issueQueueFreeList__pop[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocate
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pop[0U] 
        = vlSelfRef.__Vcellinp__issueQueueFreeList__pop
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pop[1U] 
        = vlSelfRef.__Vcellinp__issueQueueFreeList__pop
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__1(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__intIssuePtr[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuePtr
        [0U];
    vlSelfRef.__PVT__intIssuePtr[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuePtr
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__ra[0U] 
        = vlSelfRef.__PVT__intIssuePtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__ra[1U] 
        = vlSelfRef.__PVT__intIssuePtr[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__2(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__memIssuePtr[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuePtr
        [0U];
    vlSelfRef.__PVT__memIssuePtr[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuePtr
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__ra[0U] 
        = vlSelfRef.__PVT__memIssuePtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__ra[1U] 
        = vlSelfRef.__PVT__memIssuePtr[1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__3(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*3:0*/ __Vlvbound_h89188788__0;
    __Vlvbound_h89188788__0 = 0;
    // Body
    __Vlvbound_h89188788__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuePtr
        [0U];
    vlSelfRef.__PVT__complexIssuePtr[0U] = __Vlvbound_h89188788__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__ra[0U] 
        = vlSelfRef.__PVT__complexIssuePtr[0U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__4(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*3:0*/ __Vlvbound_h52962e68__0;
    __Vlvbound_h52962e68__0 = 0;
    // Body
    __Vlvbound_h52962e68__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuePtr
        [0U];
    vlSelfRef.__PVT__fpIssuePtr[0U] = __Vlvbound_h52962e68__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__ra[0U] 
        = vlSelfRef.__PVT__fpIssuePtr[0U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__5(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__5\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*5:0*/ __Vlvbound_h51cd93b2__0;
    __Vlvbound_h51cd93b2__0 = 0;
    // Body
    vlSelfRef.__PVT__selectedPtr[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr
        [0U];
    vlSelfRef.__PVT__selectedPtr[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr
        [1U];
    vlSelfRef.__PVT__selectedPtr[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr
        [2U];
    vlSelfRef.__PVT__selectedPtr[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr
        [3U];
    vlSelfRef.__PVT__selectedPtr[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr
        [4U];
    vlSelfRef.__PVT__selectedPtr[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedPtr
        [5U];
    __Vlvbound_h51cd93b2__0 = vlSelfRef.__PVT__alPtrReg
        [vlSelfRef.__PVT__selectedPtr[0U]];
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[0U] 
        = __Vlvbound_h51cd93b2__0;
    __Vlvbound_h51cd93b2__0 = vlSelfRef.__PVT__alPtrReg
        [vlSelfRef.__PVT__selectedPtr[1U]];
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[1U] 
        = __Vlvbound_h51cd93b2__0;
    __Vlvbound_h51cd93b2__0 = vlSelfRef.__PVT__alPtrReg
        [vlSelfRef.__PVT__selectedPtr[2U]];
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[2U] 
        = __Vlvbound_h51cd93b2__0;
    __Vlvbound_h51cd93b2__0 = vlSelfRef.__PVT__alPtrReg
        [vlSelfRef.__PVT__selectedPtr[3U]];
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[3U] 
        = __Vlvbound_h51cd93b2__0;
    __Vlvbound_h51cd93b2__0 = vlSelfRef.__PVT__alPtrReg
        [vlSelfRef.__PVT__selectedPtr[4U]];
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[4U] 
        = __Vlvbound_h51cd93b2__0;
    __Vlvbound_h51cd93b2__0 = vlSelfRef.__PVT__alPtrReg
        [vlSelfRef.__PVT__selectedPtr[5U]];
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__selectedActiveListPtr[5U] 
        = __Vlvbound_h51cd93b2__0;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__6(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___act_comb__TOP__SMT_RTL_Testbench__core__issueQueue__6\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__releaseEntry[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry
        [0U];
    vlSelfRef.__PVT__releaseEntry[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry
        [1U];
    vlSelfRef.__PVT__releaseEntry[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry
        [2U];
    vlSelfRef.__PVT__releaseEntry[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry
        [3U];
    vlSelfRef.__PVT__releaseEntry[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry
        [4U];
    vlSelfRef.__PVT__releaseEntry[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releaseEntry
        [5U];
    vlSelfRef.__PVT__releaseEntry[6U] = ((IData)(vlSelfRef.__PVT__issueQueueReturnIndex) 
                                         & ((IData)(vlSelfRef.__PVT__prevFlushAtRecovery) 
                                            >> (IData)(vlSelfRef.__PVT__returnIndexOffset)));
    vlSelfRef.__PVT__releaseEntry[7U] = ((IData)(vlSelfRef.__PVT__issueQueueReturnIndex) 
                                         & ((IData)(vlSelfRef.__PVT__prevFlushAtRecovery) 
                                            >> (0xfU 
                                                & ((IData)(1U) 
                                                   + (IData)(vlSelfRef.__PVT__returnIndexOffset)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[0U] 
        = vlSelfRef.__PVT__releaseEntry[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[1U] 
        = vlSelfRef.__PVT__releaseEntry[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[2U] 
        = vlSelfRef.__PVT__releaseEntry[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[3U] 
        = vlSelfRef.__PVT__releaseEntry[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[4U] 
        = vlSelfRef.__PVT__releaseEntry[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[5U] 
        = vlSelfRef.__PVT__releaseEntry[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[6U] 
        = vlSelfRef.__PVT__releaseEntry[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__push[7U] 
        = vlSelfRef.__PVT__releaseEntry[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___nba_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__0(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___nba_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vdly__freeListReset;
    __Vdly__freeListReset = 0;
    CData/*0:0*/ __Vdly__freeListResetCycleCount;
    __Vdly__freeListResetCycleCount = 0;
    CData/*0:0*/ __Vdly__issueQueueReturnIndex;
    __Vdly__issueQueueReturnIndex = 0;
    CData/*2:0*/ __Vdly__issueQueueReturnIndexCycleCount;
    __Vdly__issueQueueReturnIndexCycleCount = 0;
    CData/*5:0*/ __VdlyVal__alPtrReg__v0;
    __VdlyVal__alPtrReg__v0 = 0;
    CData/*3:0*/ __VdlyDim0__alPtrReg__v0;
    __VdlyDim0__alPtrReg__v0 = 0;
    CData/*0:0*/ __VdlySet__alPtrReg__v0;
    __VdlySet__alPtrReg__v0 = 0;
    CData/*5:0*/ __VdlyVal__alPtrReg__v1;
    __VdlyVal__alPtrReg__v1 = 0;
    CData/*3:0*/ __VdlyDim0__alPtrReg__v1;
    __VdlyDim0__alPtrReg__v1 = 0;
    CData/*0:0*/ __VdlySet__alPtrReg__v1;
    __VdlySet__alPtrReg__v1 = 0;
    CData/*0:0*/ __VdlySet__opId__v0;
    __VdlySet__opId__v0 = 0;
    SData/*11:0*/ __VdlyVal__opId__v32;
    __VdlyVal__opId__v32 = 0;
    CData/*3:0*/ __VdlyDim0__opId__v32;
    __VdlyDim0__opId__v32 = 0;
    CData/*0:0*/ __VdlySet__opId__v32;
    __VdlySet__opId__v32 = 0;
    SData/*11:0*/ __VdlyVal__opId__v33;
    __VdlyVal__opId__v33 = 0;
    CData/*3:0*/ __VdlyDim0__opId__v33;
    __VdlyDim0__opId__v33 = 0;
    CData/*0:0*/ __VdlySet__opId__v33;
    __VdlySet__opId__v33 = 0;
    // Body
    __VdlySet__opId__v0 = 0U;
    __VdlySet__opId__v32 = 0U;
    __VdlySet__opId__v33 = 0U;
    __VdlySet__alPtrReg__v0 = 0U;
    __VdlySet__alPtrReg__v1 = 0U;
    __Vdly__issueQueueReturnIndexCycleCount = vlSelfRef.__PVT__issueQueueReturnIndexCycleCount;
    __Vdly__issueQueueReturnIndex = vlSelfRef.__PVT__issueQueueReturnIndex;
    __Vdly__freeListResetCycleCount = vlSelfRef.__PVT__freeListResetCycleCount;
    __Vdly__freeListReset = vlSelfRef.__PVT__freeListReset;
    vlSelfRef.__PVT__unnamedblk8__DOT__i = 2U;
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__unnamedblk12__DOT__i = 0x10U;
        __VdlySet__opId__v0 = 1U;
        __Vdly__issueQueueReturnIndex = 0U;
        __Vdly__issueQueueReturnIndexCycleCount = 0U;
        __Vdly__freeListReset = 0U;
        __Vdly__freeListResetCycleCount = 0U;
        vlSelfRef.__PVT__prevFlushAtRecovery = 0U;
    } else {
        if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
            [0U]) {
            __VdlyVal__opId__v32 = (0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [0U][4U] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                 [0U][3U] 
                                                 >> 0x1fU)));
            __VdlyDim0__opId__v32 = vlSelfRef.__PVT__writePtr
                [0U];
            __VdlySet__opId__v32 = 1U;
        }
        if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
            [1U]) {
            __VdlyVal__opId__v33 = (0xfffU & ((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                               [1U][4U] 
                                               << 1U) 
                                              | (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
                                                 [1U][3U] 
                                                 >> 0x1fU)));
            __VdlyDim0__opId__v33 = vlSelfRef.__PVT__writePtr
                [1U];
            __VdlySet__opId__v33 = 1U;
        }
        if (((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
              >> 3U) & (0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])))) {
            __Vdly__issueQueueReturnIndex = 1U;
            __Vdly__issueQueueReturnIndexCycleCount = 0U;
            vlSelfRef.__PVT__prevFlushAtRecovery = vlSelfRef.__PVT__flush;
        } else if ((7U == (IData)(vlSelfRef.__PVT__issueQueueReturnIndexCycleCount))) {
            __Vdly__issueQueueReturnIndex = 0U;
            __Vdly__issueQueueReturnIndexCycleCount = 0U;
        } else {
            __Vdly__issueQueueReturnIndex = vlSelfRef.__PVT__issueQueueReturnIndex;
            __Vdly__issueQueueReturnIndexCycleCount 
                = (7U & ((IData)(1U) + (IData)(vlSelfRef.__PVT__issueQueueReturnIndexCycleCount)));
        }
        if (((~ (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                 >> 3U)) & (0x200000U == (0x600000U 
                                          & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])))) {
            __Vdly__freeListReset = 1U;
            __Vdly__freeListResetCycleCount = 0U;
        } else if (vlSelfRef.__PVT__freeListResetCycleCount) {
            __Vdly__freeListReset = 0U;
            __Vdly__freeListResetCycleCount = 0U;
        } else {
            __Vdly__freeListReset = vlSelfRef.__PVT__freeListReset;
            __Vdly__freeListResetCycleCount = (1U & 
                                               ((IData)(1U) 
                                                + (IData)(vlSelfRef.__PVT__freeListResetCycleCount)));
        }
    }
    if ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)))) {
        vlSelfRef.__PVT__unnamedblk13__DOT__i = 2U;
    }
    vlSelfRef.__Vcellinp__intPayloadRAM__wa[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr
        [0U];
    vlSelfRef.__Vcellinp__intPayloadRAM__wa[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr
        [1U];
    vlSelfRef.__Vcellinp__complexPayloadRAM__wa[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr
        [0U];
    vlSelfRef.__Vcellinp__complexPayloadRAM__wa[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr
        [1U];
    vlSelfRef.__Vcellinp__memPayloadRAM__wa[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr
        [0U];
    vlSelfRef.__Vcellinp__memPayloadRAM__wa[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr
        [1U];
    vlSelfRef.__Vcellinp__fpPayloadRAM__wa[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr
        [0U];
    vlSelfRef.__Vcellinp__fpPayloadRAM__wa[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr
        [1U];
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [0U]) {
        __VdlyVal__alPtrReg__v0 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeAL_Ptr
            [0U];
        __VdlyDim0__alPtrReg__v0 = vlSelfRef.__PVT__writePtr
            [0U];
        __VdlySet__alPtrReg__v0 = 1U;
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [1U]) {
        __VdlyVal__alPtrReg__v1 = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeAL_Ptr
            [1U];
        __VdlyDim0__alPtrReg__v1 = vlSelfRef.__PVT__writePtr
            [1U];
        __VdlySet__alPtrReg__v1 = 1U;
    }
    vlSelfRef.__PVT__returnIndexOffset = (((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) 
                                           | ((vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                               >> 3U) 
                                              & (0x200000U 
                                                 == 
                                                 (0x600000U 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U]))))
                                           ? 0U : (
                                                   (0xeU 
                                                    <= (IData)(vlSelfRef.__PVT__returnIndexOffset))
                                                    ? 0U
                                                    : 
                                                   (0xfU 
                                                    & ((IData)(2U) 
                                                       + (IData)(vlSelfRef.__PVT__returnIndexOffset)))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wa[0U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wa[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wa[1U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wa[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__wa[0U] 
        = vlSelfRef.__Vcellinp__complexPayloadRAM__wa
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__wa[1U] 
        = vlSelfRef.__Vcellinp__complexPayloadRAM__wa
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__wa[0U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__wa[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__wa[1U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__wa[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__wa[0U] 
        = vlSelfRef.__Vcellinp__fpPayloadRAM__wa[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__wa[1U] 
        = vlSelfRef.__Vcellinp__fpPayloadRAM__wa[1U];
    if (__VdlySet__opId__v0) {
        vlSelfRef.__PVT__opId[0U] = (3U & vlSelfRef.__PVT__opId
                                     [0U]);
        vlSelfRef.__PVT__opId[0U] = (0xffcU & vlSelfRef.__PVT__opId
                                     [0U]);
        vlSelfRef.__PVT__opId[1U] = (3U & vlSelfRef.__PVT__opId
                                     [1U]);
        vlSelfRef.__PVT__opId[1U] = (0xffcU & vlSelfRef.__PVT__opId
                                     [1U]);
        vlSelfRef.__PVT__opId[2U] = (3U & vlSelfRef.__PVT__opId
                                     [2U]);
        vlSelfRef.__PVT__opId[2U] = (0xffcU & vlSelfRef.__PVT__opId
                                     [2U]);
        vlSelfRef.__PVT__opId[3U] = (3U & vlSelfRef.__PVT__opId
                                     [3U]);
        vlSelfRef.__PVT__opId[3U] = (0xffcU & vlSelfRef.__PVT__opId
                                     [3U]);
        vlSelfRef.__PVT__opId[4U] = (3U & vlSelfRef.__PVT__opId
                                     [4U]);
        vlSelfRef.__PVT__opId[4U] = (0xffcU & vlSelfRef.__PVT__opId
                                     [4U]);
        vlSelfRef.__PVT__opId[5U] = (3U & vlSelfRef.__PVT__opId
                                     [5U]);
        vlSelfRef.__PVT__opId[5U] = (0xffcU & vlSelfRef.__PVT__opId
                                     [5U]);
        vlSelfRef.__PVT__opId[6U] = (3U & vlSelfRef.__PVT__opId
                                     [6U]);
        vlSelfRef.__PVT__opId[6U] = (0xffcU & vlSelfRef.__PVT__opId
                                     [6U]);
        vlSelfRef.__PVT__opId[7U] = (3U & vlSelfRef.__PVT__opId
                                     [7U]);
        vlSelfRef.__PVT__opId[7U] = (0xffcU & vlSelfRef.__PVT__opId
                                     [7U]);
        vlSelfRef.__PVT__opId[8U] = (3U & vlSelfRef.__PVT__opId
                                     [8U]);
        vlSelfRef.__PVT__opId[8U] = (0xffcU & vlSelfRef.__PVT__opId
                                     [8U]);
        vlSelfRef.__PVT__opId[9U] = (3U & vlSelfRef.__PVT__opId
                                     [9U]);
        vlSelfRef.__PVT__opId[9U] = (0xffcU & vlSelfRef.__PVT__opId
                                     [9U]);
        vlSelfRef.__PVT__opId[0xaU] = (3U & vlSelfRef.__PVT__opId
                                       [0xaU]);
        vlSelfRef.__PVT__opId[0xaU] = (0xffcU & vlSelfRef.__PVT__opId
                                       [0xaU]);
        vlSelfRef.__PVT__opId[0xbU] = (3U & vlSelfRef.__PVT__opId
                                       [0xbU]);
        vlSelfRef.__PVT__opId[0xbU] = (0xffcU & vlSelfRef.__PVT__opId
                                       [0xbU]);
        vlSelfRef.__PVT__opId[0xcU] = (3U & vlSelfRef.__PVT__opId
                                       [0xcU]);
        vlSelfRef.__PVT__opId[0xcU] = (0xffcU & vlSelfRef.__PVT__opId
                                       [0xcU]);
        vlSelfRef.__PVT__opId[0xdU] = (3U & vlSelfRef.__PVT__opId
                                       [0xdU]);
        vlSelfRef.__PVT__opId[0xdU] = (0xffcU & vlSelfRef.__PVT__opId
                                       [0xdU]);
        vlSelfRef.__PVT__opId[0xeU] = (3U & vlSelfRef.__PVT__opId
                                       [0xeU]);
        vlSelfRef.__PVT__opId[0xeU] = (0xffcU & vlSelfRef.__PVT__opId
                                       [0xeU]);
        vlSelfRef.__PVT__opId[0xfU] = (3U & vlSelfRef.__PVT__opId
                                       [0xfU]);
        vlSelfRef.__PVT__opId[0xfU] = (0xffcU & vlSelfRef.__PVT__opId
                                       [0xfU]);
    }
    if (__VdlySet__opId__v32) {
        vlSelfRef.__PVT__opId[__VdlyDim0__opId__v32] 
            = __VdlyVal__opId__v32;
    }
    if (__VdlySet__opId__v33) {
        vlSelfRef.__PVT__opId[__VdlyDim0__opId__v33] 
            = __VdlyVal__opId__v33;
    }
    vlSelfRef.__PVT__writePtr[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr
        [0U];
    vlSelfRef.__PVT__writePtr[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writePtr
        [1U];
    if (__VdlySet__alPtrReg__v0) {
        vlSelfRef.__PVT__alPtrReg[__VdlyDim0__alPtrReg__v0] 
            = __VdlyVal__alPtrReg__v0;
    }
    if (__VdlySet__alPtrReg__v1) {
        vlSelfRef.__PVT__alPtrReg[__VdlyDim0__alPtrReg__v1] 
            = __VdlyVal__alPtrReg__v1;
    }
    vlSelfRef.__PVT__issueQueueReturnIndexCycleCount 
        = __Vdly__issueQueueReturnIndexCycleCount;
    vlSelfRef.__PVT__issueQueueReturnIndex = __Vdly__issueQueueReturnIndex;
    vlSelfRef.__PVT__freeListResetCycleCount = __Vdly__freeListResetCycleCount;
    vlSelfRef.__PVT__freeListReset = __Vdly__freeListReset;
    vlSelfRef.__PVT__releasePtr[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releasePtr
        [0U];
    vlSelfRef.__PVT__releasePtr[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releasePtr
        [1U];
    vlSelfRef.__PVT__releasePtr[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releasePtr
        [2U];
    vlSelfRef.__PVT__releasePtr[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releasePtr
        [3U];
    vlSelfRef.__PVT__releasePtr[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releasePtr
        [4U];
    vlSelfRef.__PVT__releasePtr[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__releasePtr
        [5U];
    vlSelfRef.__PVT__releasePtr[6U] = vlSelfRef.__PVT__returnIndexOffset;
    vlSelfRef.__PVT__releasePtr[7U] = (0xfU & (((IData)(vlSelfRef.__PVT__issueQueueReturnIndex) 
                                                & ((IData)(vlSelfRef.__PVT__prevFlushAtRecovery) 
                                                   >> 
                                                   (0xfU 
                                                    & ((IData)(1U) 
                                                       + (IData)(vlSelfRef.__PVT__returnIndexOffset)))))
                                                ? ((IData)(1U) 
                                                   + (IData)(vlSelfRef.__PVT__returnIndexOffset))
                                                : ((IData)(1U) 
                                                   + (IData)(vlSelfRef.__PVT__returnIndexOffset))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pushedData[0U] 
        = vlSelfRef.__PVT__releasePtr[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pushedData[1U] 
        = vlSelfRef.__PVT__releasePtr[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pushedData[2U] 
        = vlSelfRef.__PVT__releasePtr[2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pushedData[3U] 
        = vlSelfRef.__PVT__releasePtr[3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pushedData[4U] 
        = vlSelfRef.__PVT__releasePtr[4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pushedData[5U] 
        = vlSelfRef.__PVT__releasePtr[5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pushedData[6U] 
        = vlSelfRef.__PVT__releasePtr[6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__pushedData[7U] 
        = vlSelfRef.__PVT__releasePtr[7U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___nba_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__1(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___nba_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__Vcellinp__intPayloadRAM__wv[0U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
        [0U][0U];
    vlSelfRef.__Vcellinp__intPayloadRAM__wv[0U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
        [0U][1U];
    vlSelfRef.__Vcellinp__intPayloadRAM__wv[0U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
        [0U][2U];
    vlSelfRef.__Vcellinp__intPayloadRAM__wv[0U][3U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
        [0U][3U];
    vlSelfRef.__Vcellinp__intPayloadRAM__wv[0U][4U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
        [0U][4U];
    vlSelfRef.__Vcellinp__intPayloadRAM__wv[1U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
        [1U][0U];
    vlSelfRef.__Vcellinp__intPayloadRAM__wv[1U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
        [1U][1U];
    vlSelfRef.__Vcellinp__intPayloadRAM__wv[1U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
        [1U][2U];
    vlSelfRef.__Vcellinp__intPayloadRAM__wv[1U][3U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
        [1U][3U];
    vlSelfRef.__Vcellinp__intPayloadRAM__wv[1U][4U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intWriteData
        [1U][4U];
    vlSelfRef.__Vcellinp__complexPayloadRAM__wv[0U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexWriteData
        [0U][0U];
    vlSelfRef.__Vcellinp__complexPayloadRAM__wv[0U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexWriteData
        [0U][1U];
    vlSelfRef.__Vcellinp__complexPayloadRAM__wv[0U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexWriteData
        [0U][2U];
    vlSelfRef.__Vcellinp__complexPayloadRAM__wv[1U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexWriteData
        [1U][0U];
    vlSelfRef.__Vcellinp__complexPayloadRAM__wv[1U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexWriteData
        [1U][1U];
    vlSelfRef.__Vcellinp__complexPayloadRAM__wv[1U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexWriteData
        [1U][2U];
    vlSelfRef.__Vcellinp__memPayloadRAM__wv[0U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memWriteData
        [0U][0U];
    vlSelfRef.__Vcellinp__memPayloadRAM__wv[0U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memWriteData
        [0U][1U];
    vlSelfRef.__Vcellinp__memPayloadRAM__wv[0U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memWriteData
        [0U][2U];
    vlSelfRef.__Vcellinp__memPayloadRAM__wv[0U][3U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memWriteData
        [0U][3U];
    vlSelfRef.__Vcellinp__memPayloadRAM__wv[1U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memWriteData
        [1U][0U];
    vlSelfRef.__Vcellinp__memPayloadRAM__wv[1U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memWriteData
        [1U][1U];
    vlSelfRef.__Vcellinp__memPayloadRAM__wv[1U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memWriteData
        [1U][2U];
    vlSelfRef.__Vcellinp__memPayloadRAM__wv[1U][3U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memWriteData
        [1U][3U];
    vlSelfRef.__Vcellinp__fpPayloadRAM__wv[0U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpWriteData
        [0U][0U];
    vlSelfRef.__Vcellinp__fpPayloadRAM__wv[0U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpWriteData
        [0U][1U];
    vlSelfRef.__Vcellinp__fpPayloadRAM__wv[0U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpWriteData
        [0U][2U];
    vlSelfRef.__Vcellinp__fpPayloadRAM__wv[1U][0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpWriteData
        [1U][0U];
    vlSelfRef.__Vcellinp__fpPayloadRAM__wv[1U][1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpWriteData
        [1U][1U];
    vlSelfRef.__Vcellinp__fpPayloadRAM__wv[1U][2U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpWriteData
        [1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wv[0U][0U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wv[0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wv[0U][1U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wv[0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wv[0U][2U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wv[0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wv[0U][3U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wv[0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wv[0U][4U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wv[0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wv[1U][0U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wv[1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wv[1U][1U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wv[1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wv[1U][2U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wv[1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wv[1U][3U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wv[1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__intPayloadRAM.__PVT__wv[1U][4U] 
        = vlSelfRef.__Vcellinp__intPayloadRAM__wv[1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__wv[0U][0U] 
        = vlSelfRef.__Vcellinp__complexPayloadRAM__wv
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__wv[0U][1U] 
        = vlSelfRef.__Vcellinp__complexPayloadRAM__wv
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__wv[0U][2U] 
        = vlSelfRef.__Vcellinp__complexPayloadRAM__wv
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__wv[1U][0U] 
        = vlSelfRef.__Vcellinp__complexPayloadRAM__wv
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__wv[1U][1U] 
        = vlSelfRef.__Vcellinp__complexPayloadRAM__wv
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__complexPayloadRAM.__PVT__wv[1U][2U] 
        = vlSelfRef.__Vcellinp__complexPayloadRAM__wv
        [1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__wv[0U][0U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__wv[0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__wv[0U][1U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__wv[0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__wv[0U][2U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__wv[0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__wv[0U][3U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__wv[0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__wv[1U][0U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__wv[1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__wv[1U][1U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__wv[1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__wv[1U][2U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__wv[1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__memPayloadRAM.__PVT__wv[1U][3U] 
        = vlSelfRef.__Vcellinp__memPayloadRAM__wv[1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__wv[0U][0U] 
        = vlSelfRef.__Vcellinp__fpPayloadRAM__wv[0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__wv[0U][1U] 
        = vlSelfRef.__Vcellinp__fpPayloadRAM__wv[0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__wv[0U][2U] 
        = vlSelfRef.__Vcellinp__fpPayloadRAM__wv[0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__wv[1U][0U] 
        = vlSelfRef.__Vcellinp__fpPayloadRAM__wv[1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__wv[1U][1U] 
        = vlSelfRef.__Vcellinp__fpPayloadRAM__wv[1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__fpPayloadRAM.__PVT__wv[1U][2U] 
        = vlSelfRef.__Vcellinp__fpPayloadRAM__wv[1U][2U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_IssueQueue___nba_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__2(VSMT_RTL_Testbench_IssueQueue* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+                VSMT_RTL_Testbench_IssueQueue___nba_sequent__TOP__SMT_RTL_Testbench__core__issueQueue__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__0__detectRange;
    __Vfunc_SelectiveFlushDetector__0__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__0__headPtr;
    __Vfunc_SelectiveFlushDetector__0__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__0__tailPtr;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__0__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__0__opPtr;
    __Vfunc_SelectiveFlushDetector__0__opPtr = 0;
    // Body
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [0U];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel1;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel1;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel1;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel1;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel1;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel1;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel1;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel1: ;
    }
    vlSelfRef.__PVT__flush = ((0xfffeU & (IData)(vlSelfRef.__PVT__flush)) 
                              | (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [1U];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 1U)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel2;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel2;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel2;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel2;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel2;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel2;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel2;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel2: ;
    }
    vlSelfRef.__PVT__flush = ((0xfffdU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 1U));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [2U];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 2U)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel3;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel3;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel3;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel3;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel3;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel3;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel3;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel3: ;
    }
    vlSelfRef.__PVT__flush = ((0xfffbU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 2U));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [3U];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 3U)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel4;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel4;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel4;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel4;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel4;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel4;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel4;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel4: ;
    }
    vlSelfRef.__PVT__flush = ((0xfff7U & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 3U));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [4U];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 4U)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel5;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel5;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel5;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel5;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel5;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel5;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel5;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel5: ;
    }
    vlSelfRef.__PVT__flush = ((0xffefU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 4U));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [5U];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 5U)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel6;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel6;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel6;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel6;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel6;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel6;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel6;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel6: ;
    }
    vlSelfRef.__PVT__flush = ((0xffdfU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 5U));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [6U];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 6U)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel7;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel7;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel7;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel7;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel7;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel7;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel7;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel7: ;
    }
    vlSelfRef.__PVT__flush = ((0xffbfU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 6U));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [7U];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 7U)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel8;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel8;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel8;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel8;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel8;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel8;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel8;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel8: ;
    }
    vlSelfRef.__PVT__flush = ((0xff7fU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 7U));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [8U];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 8U)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel9;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel9;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel9;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel9;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel9;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel9;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel9;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel9: ;
    }
    vlSelfRef.__PVT__flush = ((0xfeffU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 8U));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [9U];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 9U)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel10;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel10;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel10;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel10;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel10;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel10;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel10;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel10: ;
    }
    vlSelfRef.__PVT__flush = ((0xfdffU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 9U));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [0xaU];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 0xaU)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel11;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel11;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel11;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel11;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel11;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel11;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel11;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel11: ;
    }
    vlSelfRef.__PVT__flush = ((0xfbffU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 0xaU));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [0xbU];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 0xbU)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel12;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel12;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel12;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel12;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel12;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel12;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel12;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel12: ;
    }
    vlSelfRef.__PVT__flush = ((0xf7ffU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 0xbU));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [0xcU];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 0xcU)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel13;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel13;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel13;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel13;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel13;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel13;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel13;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel13: ;
    }
    vlSelfRef.__PVT__flush = ((0xefffU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 0xcU));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [0xdU];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 0xdU)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel14;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel14;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel14;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel14;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel14;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel14;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel14;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel14: ;
    }
    vlSelfRef.__PVT__flush = ((0xdfffU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 0xdU));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [0xeU];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 0xeU)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel15;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel15;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel15;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel15;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel15;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel15;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel15;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel15: ;
    }
    vlSelfRef.__PVT__flush = ((0xbfffU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 0xeU));
    __Vfunc_SelectiveFlushDetector__0__opPtr = vlSelfRef.__PVT__alPtrReg
        [0xfU];
    __Vfunc_SelectiveFlushDetector__0__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__0__tailPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 4U));
    __Vfunc_SelectiveFlushDetector__0__headPtr = (0x3fU 
                                                  & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                                                     >> 0xaU));
    __Vfunc_SelectiveFlushDetector__0__detectRange 
        = (IData)(((0x200000U == (0x600000U & vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U])) 
                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__scheduler__DOT__notIssued) 
                      >> 0xfU)));
    {
        if (__Vfunc_SelectiveFlushDetector__0__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__0__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                goto __Vlabel16;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel16;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel16;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__0__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel16;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__0__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__0__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__0__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 1U;
                    goto __Vlabel16;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                    goto __Vlabel16;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
                goto __Vlabel16;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout = 0U;
        }
        __Vlabel16: ;
    }
    vlSelfRef.__PVT__flush = ((0x7fffU & (IData)(vlSelfRef.__PVT__flush)) 
                              | ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__0__Vfuncout) 
                                 << 0xfU));
    vlSelfRef.__PVT__unnamedblk10__DOT__i = 0x10U;
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocated
        [0U]) {
        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeAL_Ptr
            [0U];
        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__flushAllInsns 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr 
            = (0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                        >> 4U));
        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr 
            = (0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                        >> 0xaU));
        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__detectRange 
            = (1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                            >> 0x15U)));
        {
            if (vlSelfRef.__Vfunc_SelectiveFlushDetector__1__detectRange) {
                if (vlSelfRef.__Vfunc_SelectiveFlushDetector__1__flushAllInsns) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 1U;
                    goto __Vlabel17;
                } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__detectRange) 
                            & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr) 
                               >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr)))) {
                    if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                          >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr)) 
                         & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                            < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr)))) {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 1U;
                        goto __Vlabel17;
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 0U;
                        goto __Vlabel17;
                    }
                } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__detectRange) 
                            & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr) 
                               < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr)))) {
                    if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                          >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr)) 
                         & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                            > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr)))) {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 1U;
                        goto __Vlabel17;
                    } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                                 < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr)) 
                                & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                                   < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr)))) {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 1U;
                        goto __Vlabel17;
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 0U;
                        goto __Vlabel17;
                    }
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 0U;
                    goto __Vlabel17;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 0U;
            }
            __Vlabel17: ;
        }
        vlSelfRef.__PVT__flush = (((~ ((IData)(1U) 
                                       << vlSelfRef.__PVT__writePtr
                                       [0U])) & (IData)(vlSelfRef.__PVT__flush)) 
                                  | (0xffffU & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout) 
                                                << 
                                                vlSelfRef.__PVT__writePtr
                                                [0U])));
    }
    if (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocated
        [1U]) {
        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeAL_Ptr
            [1U];
        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__flushAllInsns 
            = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr 
            = (0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                        >> 4U));
        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr 
            = (0x3fU & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[0U] 
                        >> 0xaU));
        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__detectRange 
            = (1U == (3U & (vlSymsp->TOP__SMT_RTL_Testbench__core.__PVT__recoveryManager__DOT__regState[3U] 
                            >> 0x15U)));
        {
            if (vlSelfRef.__Vfunc_SelectiveFlushDetector__1__detectRange) {
                if (vlSelfRef.__Vfunc_SelectiveFlushDetector__1__flushAllInsns) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 1U;
                    goto __Vlabel18;
                } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__detectRange) 
                            & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr) 
                               >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr)))) {
                    if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                          >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr)) 
                         & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                            < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr)))) {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 1U;
                        goto __Vlabel18;
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 0U;
                        goto __Vlabel18;
                    }
                } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__detectRange) 
                            & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr) 
                               < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr)))) {
                    if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                          >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr)) 
                         & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                            > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr)))) {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 1U;
                        goto __Vlabel18;
                    } else if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                                 < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__headPtr)) 
                                & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__opPtr) 
                                   < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__tailPtr)))) {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 1U;
                        goto __Vlabel18;
                    } else {
                        vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 0U;
                        goto __Vlabel18;
                    }
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 0U;
                    goto __Vlabel18;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout = 0U;
            }
            __Vlabel18: ;
        }
        vlSelfRef.__PVT__flush = (((~ ((IData)(1U) 
                                       << vlSelfRef.__PVT__writePtr
                                       [1U])) & (IData)(vlSelfRef.__PVT__flush)) 
                                  | (0xffffU & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__1__Vfuncout) 
                                                << 
                                                vlSelfRef.__PVT__writePtr
                                                [1U])));
    }
    vlSelfRef.__PVT__unnamedblk11__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushIQ_Entry 
        = vlSelfRef.__PVT__flush;
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__issueQueueReturnIndex 
        = ((IData)(vlSelfRef.__PVT__issueQueueReturnIndex) 
           | (IData)(vlSelfRef.__PVT__freeListReset));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                << 0xcU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0U]) | vlSelfRef.__PVT__opId[0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[1U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [1U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                << 0xbU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[1U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [1U]) | vlSelfRef.__PVT__opId[1U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[2U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [2U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                << 0xaU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[2U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [2U]) | vlSelfRef.__PVT__opId[2U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[3U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [3U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                << 9U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[3U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [3U]) | vlSelfRef.__PVT__opId[3U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[4U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [4U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                << 8U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[4U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [4U]) | vlSelfRef.__PVT__opId[4U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[5U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [5U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                << 7U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[5U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [5U]) | vlSelfRef.__PVT__opId[5U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[6U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [6U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                << 6U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[6U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [6U]) | vlSelfRef.__PVT__opId[6U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[7U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [7U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                << 5U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[7U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [7U]) | vlSelfRef.__PVT__opId[7U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[8U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [8U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                << 4U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[8U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [8U]) | vlSelfRef.__PVT__opId[8U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[9U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [9U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                << 3U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[9U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [9U]) | vlSelfRef.__PVT__opId[9U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xaU] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xaU]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                  << 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xaU] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xaU]) | vlSelfRef.__PVT__opId[0xaU]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xbU] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xbU]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                  << 1U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xbU] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xbU]) | vlSelfRef.__PVT__opId[0xbU]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xcU] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xcU]) | (0x1000U & (IData)(vlSelfRef.__PVT__flush)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xcU] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xcU]) | vlSelfRef.__PVT__opId[0xcU]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xdU] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xdU]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                  >> 1U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xdU] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xdU]) | vlSelfRef.__PVT__opId[0xdU]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xeU] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xeU]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                  >> 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xeU] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xeU]) | vlSelfRef.__PVT__opId[0xeU]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xfU] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xfU]) | (0x1000U & ((IData)(vlSelfRef.__PVT__flush) 
                                  >> 3U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue[0xfU] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
            [0xfU]) | vlSelfRef.__PVT__opId[0xfU]);
}
