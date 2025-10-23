// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VSMT_RTL_Testbench.h for the primary calling header

#include "VSMT_RTL_Testbench__pch.h"
#include "VSMT_RTL_Testbench_Core.h"
#include "VSMT_RTL_Testbench__Syms.h"

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___ico_sequent__TOP__SMT_RTL_Testbench__core__0(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___ico_sequent__TOP__SMT_RTL_Testbench__core__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    SData/*10:0*/ debug__DOT____Vlvbound_ha52f74cc__0;
    debug__DOT____Vlvbound_ha52f74cc__0 = 0;
    SData/*12:0*/ debug__DOT____Vlvbound_h298aa480__0;
    debug__DOT____Vlvbound_h298aa480__0 = 0;
    VlWide<3>/*80:0*/ debug__DOT____Vlvbound_hcde55a48__0;
    VL_ZERO_W(81, debug__DOT____Vlvbound_hcde55a48__0);
    IData/*17:0*/ debug__DOT____Vlvbound_h710e7aad__0;
    debug__DOT____Vlvbound_h710e7aad__0 = 0;
    SData/*12:0*/ debug__DOT____Vlvbound_h26b82c2f__0;
    debug__DOT____Vlvbound_h26b82c2f__0 = 0;
    VlWide<3>/*85:0*/ debug__DOT____Vlvbound_h3e5487b7__0;
    VL_ZERO_W(86, debug__DOT____Vlvbound_h3e5487b7__0);
    SData/*13:0*/ debug__DOT____Vlvbound_ha11163d0__0;
    debug__DOT____Vlvbound_ha11163d0__0 = 0;
    SData/*13:0*/ debug__DOT____Vlvbound_h9f94c590__0;
    debug__DOT____Vlvbound_h9f94c590__0 = 0;
    VlWide<4>/*117:0*/ debug__DOT____Vlvbound_h92a32f7c__0;
    VL_ZERO_W(118, debug__DOT____Vlvbound_h92a32f7c__0);
    SData/*13:0*/ debug__DOT____Vlvbound_ha3a841b2__0;
    debug__DOT____Vlvbound_ha3a841b2__0 = 0;
    SData/*13:0*/ debug__DOT____Vlvbound_hf7de855a__0;
    debug__DOT____Vlvbound_hf7de855a__0 = 0;
    SData/*13:0*/ debug__DOT____Vlvbound_hf74e2cd4__0;
    debug__DOT____Vlvbound_hf74e2cd4__0 = 0;
    VlWide<5>/*137:0*/ debug__DOT____Vlvbound_h4c346d8f__0;
    VL_ZERO_W(138, debug__DOT____Vlvbound_h4c346d8f__0);
    SData/*13:0*/ debug__DOT____Vlvbound_h907743c3__0;
    debug__DOT____Vlvbound_h907743c3__0 = 0;
    SData/*13:0*/ debug__DOT____Vlvbound_h902d8323__0;
    debug__DOT____Vlvbound_h902d8323__0 = 0;
    SData/*13:0*/ debug__DOT____Vlvbound_h8fd8a9d3__0;
    debug__DOT____Vlvbound_h8fd8a9d3__0 = 0;
    VlWide<4>/*115:0*/ debug__DOT____Vlvbound_h64dfedfa__0;
    VL_ZERO_W(116, debug__DOT____Vlvbound_h64dfedfa__0);
    VlWide<6>/*174:0*/ debug__DOT____Vlvbound_h05d1cf60__0;
    VL_ZERO_W(175, debug__DOT____Vlvbound_h05d1cf60__0);
    VlWide<9>/*273:0*/ debug__DOT____Vlvbound_h02afb21f__0;
    VL_ZERO_W(274, debug__DOT____Vlvbound_h02afb21f__0);
    SData/*13:0*/ debug__DOT____Vlvbound_h3dbf3c99__0;
    debug__DOT____Vlvbound_h3dbf3c99__0 = 0;
    SData/*13:0*/ debug__DOT____Vlvbound_h376f222d__0;
    debug__DOT____Vlvbound_h376f222d__0 = 0;
    SData/*13:0*/ debug__DOT____Vlvbound_h3785688d__0;
    debug__DOT____Vlvbound_h3785688d__0 = 0;
    VlWide<7>/*197:0*/ debug__DOT____Vlvbound_h6455cb13__0;
    VL_ZERO_W(198, debug__DOT____Vlvbound_h6455cb13__0);
    SData/*13:0*/ debug__DOT____Vlvbound_h9954e88d__0;
    debug__DOT____Vlvbound_h9954e88d__0 = 0;
    IData/*21:0*/ debug__DOT____Vlvbound_h322a76c3__0;
    debug__DOT____Vlvbound_h322a76c3__0 = 0;
    CData/*0:0*/ debug__DOT____Vlvbound_h4ff58c65__0;
    debug__DOT____Vlvbound_h4ff58c65__0 = 0;
    SData/*12:0*/ debug__DOT____Vlvbound_h419daeb4__0;
    debug__DOT____Vlvbound_h419daeb4__0 = 0;
    // Body
    debug__DOT____Vlvbound_ha52f74cc__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x5aU] = ((0xfffffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x5aU]) 
                                                | ((IData)(debug__DOT____Vlvbound_ha52f74cc__0) 
                                                   << 0x1cU));
    vlSelfRef.__PVT__debug__DOT__next[0x5bU] = ((0x3ff80U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x5bU]) 
                                                | (0x3ffffU 
                                                   & ((IData)(debug__DOT____Vlvbound_ha52f74cc__0) 
                                                      >> 4U)));
    debug__DOT____Vlvbound_h298aa480__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x5aU] = ((0xffff8003U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x5aU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h298aa480__0) 
                                                   << 2U));
    debug__DOT____Vlvbound_ha52f74cc__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0x5bU] = ((0x7fU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x5bU]) 
                                                | (0x3ffffU 
                                                   & ((IData)(debug__DOT____Vlvbound_ha52f74cc__0) 
                                                      << 7U)));
    debug__DOT____Vlvbound_h298aa480__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0x5aU] = ((0xf0007fffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x5aU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h298aa480__0) 
                                                   << 0xfU));
    debug__DOT____Vlvbound_hcde55a48__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
        [0U][0U];
    debug__DOT____Vlvbound_hcde55a48__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
        [0U][1U];
    debug__DOT____Vlvbound_hcde55a48__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
        [0U][2U];
    vlSelfRef.__PVT__debug__DOT__next[0x53U] = ((0xfffffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x53U]) 
                                                | (debug__DOT____Vlvbound_hcde55a48__0[0U] 
                                                   << 0x1cU));
    vlSelfRef.__PVT__debug__DOT__next[0x54U] = ((debug__DOT____Vlvbound_hcde55a48__0[0U] 
                                                 >> 4U) 
                                                | (debug__DOT____Vlvbound_hcde55a48__0[1U] 
                                                   << 0x1cU));
    vlSelfRef.__PVT__debug__DOT__next[0x55U] = ((debug__DOT____Vlvbound_hcde55a48__0[1U] 
                                                 >> 4U) 
                                                | (debug__DOT____Vlvbound_hcde55a48__0[2U] 
                                                   << 0x1cU));
    vlSelfRef.__PVT__debug__DOT__next[0x56U] = ((0xffffe000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x56U]) 
                                                | (debug__DOT____Vlvbound_hcde55a48__0[2U] 
                                                   >> 4U));
    debug__DOT____Vlvbound_hcde55a48__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
        [1U][0U];
    debug__DOT____Vlvbound_hcde55a48__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
        [1U][1U];
    debug__DOT____Vlvbound_hcde55a48__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idReg
        [1U][2U];
    vlSelfRef.__PVT__debug__DOT__next[0x56U] = ((0x1fffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x56U]) 
                                                | (debug__DOT____Vlvbound_hcde55a48__0[0U] 
                                                   << 0xdU));
    vlSelfRef.__PVT__debug__DOT__next[0x57U] = ((debug__DOT____Vlvbound_hcde55a48__0[0U] 
                                                 >> 0x13U) 
                                                | (debug__DOT____Vlvbound_hcde55a48__0[1U] 
                                                   << 0xdU));
    vlSelfRef.__PVT__debug__DOT__next[0x58U] = ((0xc0000000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x58U]) 
                                                | ((debug__DOT____Vlvbound_hcde55a48__0[1U] 
                                                    >> 0x13U) 
                                                   | (debug__DOT____Vlvbound_hcde55a48__0[2U] 
                                                      << 0xdU)));
    debug__DOT____Vlvbound_h710e7aad__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__pdReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x58U] = ((0x3fffffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x58U]) 
                                                | (debug__DOT____Vlvbound_h710e7aad__0 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x59U] = ((0xffff0000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x59U]) 
                                                | (debug__DOT____Vlvbound_h710e7aad__0 
                                                   >> 2U));
    debug__DOT____Vlvbound_h710e7aad__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__pdReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0x59U] = ((0xffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x59U]) 
                                                | (debug__DOT____Vlvbound_h710e7aad__0 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x5aU] = ((0xfffffffcU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x5aU]) 
                                                | (debug__DOT____Vlvbound_h710e7aad__0 
                                                   >> 0x10U));
    debug__DOT____Vlvbound_h26b82c2f__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x53U] = ((0xffff8003U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x53U]) 
                                                | ((IData)(debug__DOT____Vlvbound_h26b82c2f__0) 
                                                   << 2U));
    debug__DOT____Vlvbound_h26b82c2f__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0x53U] = ((0xf0007fffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x53U]) 
                                                | ((IData)(debug__DOT____Vlvbound_h26b82c2f__0) 
                                                   << 0xfU));
    debug__DOT____Vlvbound_h3e5487b7__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__dsReg
        [0U][0U];
    debug__DOT____Vlvbound_h3e5487b7__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__dsReg
        [0U][1U];
    debug__DOT____Vlvbound_h3e5487b7__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__dsReg
        [0U][2U];
    vlSelfRef.__PVT__debug__DOT__next[0x4dU] = ((0x3fffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x4dU]) 
                                                | (debug__DOT____Vlvbound_h3e5487b7__0[0U] 
                                                   << 0x16U));
    vlSelfRef.__PVT__debug__DOT__next[0x4eU] = ((debug__DOT____Vlvbound_h3e5487b7__0[0U] 
                                                 >> 0xaU) 
                                                | (debug__DOT____Vlvbound_h3e5487b7__0[1U] 
                                                   << 0x16U));
    vlSelfRef.__PVT__debug__DOT__next[0x4fU] = ((debug__DOT____Vlvbound_h3e5487b7__0[1U] 
                                                 >> 0xaU) 
                                                | (debug__DOT____Vlvbound_h3e5487b7__0[2U] 
                                                   << 0x16U));
    vlSelfRef.__PVT__debug__DOT__next[0x50U] = ((0xfffff000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x50U]) 
                                                | (debug__DOT____Vlvbound_h3e5487b7__0[2U] 
                                                   >> 0xaU));
    debug__DOT____Vlvbound_h3e5487b7__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__dsReg
        [1U][0U];
    debug__DOT____Vlvbound_h3e5487b7__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__dsReg
        [1U][1U];
    debug__DOT____Vlvbound_h3e5487b7__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__dsReg
        [1U][2U];
    vlSelfRef.__PVT__debug__DOT__next[0x50U] = ((0xfffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x50U]) 
                                                | (debug__DOT____Vlvbound_h3e5487b7__0[0U] 
                                                   << 0xcU));
    vlSelfRef.__PVT__debug__DOT__next[0x51U] = ((debug__DOT____Vlvbound_h3e5487b7__0[0U] 
                                                 >> 0x14U) 
                                                | (debug__DOT____Vlvbound_h3e5487b7__0[1U] 
                                                   << 0xcU));
    vlSelfRef.__PVT__debug__DOT__next[0x52U] = ((debug__DOT____Vlvbound_h3e5487b7__0[1U] 
                                                 >> 0x14U) 
                                                | (debug__DOT____Vlvbound_h3e5487b7__0[2U] 
                                                   << 0xcU));
    vlSelfRef.__PVT__debug__DOT__next[0x53U] = ((0xfffffffcU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x53U]) 
                                                | (debug__DOT____Vlvbound_h3e5487b7__0[2U] 
                                                   >> 0x14U));
    debug__DOT____Vlvbound_ha11163d0__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x4cU] = ((0x3ffffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x4cU]) 
                                                | ((IData)(debug__DOT____Vlvbound_ha11163d0__0) 
                                                   << 0x1aU));
    vlSelfRef.__PVT__debug__DOT__next[0x4dU] = ((0xffffff00U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x4dU]) 
                                                | ((IData)(debug__DOT____Vlvbound_ha11163d0__0) 
                                                   >> 6U));
    debug__DOT____Vlvbound_h9f94c590__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x4bU] = ((0x3fffffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x4bU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h9f94c590__0) 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x4cU] = ((0xfffff000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x4cU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h9f94c590__0) 
                                                   >> 2U));
    debug__DOT____Vlvbound_h92a32f7c__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
        [0U][0U];
    debug__DOT____Vlvbound_h92a32f7c__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
        [0U][1U];
    debug__DOT____Vlvbound_h92a32f7c__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
        [0U][2U];
    debug__DOT____Vlvbound_h92a32f7c__0[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
        [0U][3U];
    vlSelfRef.__PVT__debug__DOT__next[0x44U] = ((0x3ffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x44U]) 
                                                | (debug__DOT____Vlvbound_h92a32f7c__0[0U] 
                                                   << 0x12U));
    vlSelfRef.__PVT__debug__DOT__next[0x45U] = ((debug__DOT____Vlvbound_h92a32f7c__0[0U] 
                                                 >> 0xeU) 
                                                | (debug__DOT____Vlvbound_h92a32f7c__0[1U] 
                                                   << 0x12U));
    vlSelfRef.__PVT__debug__DOT__next[0x46U] = ((debug__DOT____Vlvbound_h92a32f7c__0[1U] 
                                                 >> 0xeU) 
                                                | (debug__DOT____Vlvbound_h92a32f7c__0[2U] 
                                                   << 0x12U));
    vlSelfRef.__PVT__debug__DOT__next[0x47U] = ((debug__DOT____Vlvbound_h92a32f7c__0[2U] 
                                                 >> 0xeU) 
                                                | (debug__DOT____Vlvbound_h92a32f7c__0[3U] 
                                                   << 0x12U));
    vlSelfRef.__PVT__debug__DOT__next[0x48U] = ((0xffffff00U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x48U]) 
                                                | (debug__DOT____Vlvbound_h92a32f7c__0[3U] 
                                                   >> 0xeU));
    debug__DOT____Vlvbound_ha3a841b2__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x43U] = ((0x3fffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x43U]) 
                                                | ((IData)(debug__DOT____Vlvbound_ha3a841b2__0) 
                                                   << 0x16U));
    vlSelfRef.__PVT__debug__DOT__next[0x44U] = ((0xfffffff0U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x44U]) 
                                                | ((IData)(debug__DOT____Vlvbound_ha3a841b2__0) 
                                                   >> 0xaU));
    debug__DOT____Vlvbound_ha11163d0__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intIsReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0x4dU] = ((0xffc000ffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x4dU]) 
                                                | ((IData)(debug__DOT____Vlvbound_ha11163d0__0) 
                                                   << 8U));
    debug__DOT____Vlvbound_h9f94c590__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0x4cU] = ((0xfc000fffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x4cU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h9f94c590__0) 
                                                   << 0xcU));
    debug__DOT____Vlvbound_h92a32f7c__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
        [1U][0U];
    debug__DOT____Vlvbound_h92a32f7c__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
        [1U][1U];
    debug__DOT____Vlvbound_h92a32f7c__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
        [1U][2U];
    debug__DOT____Vlvbound_h92a32f7c__0[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intExReg
        [1U][3U];
    vlSelfRef.__PVT__debug__DOT__next[0x48U] = ((0xffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x48U]) 
                                                | (debug__DOT____Vlvbound_h92a32f7c__0[0U] 
                                                   << 8U));
    vlSelfRef.__PVT__debug__DOT__next[0x49U] = ((debug__DOT____Vlvbound_h92a32f7c__0[0U] 
                                                 >> 0x18U) 
                                                | (debug__DOT____Vlvbound_h92a32f7c__0[1U] 
                                                   << 8U));
    vlSelfRef.__PVT__debug__DOT__next[0x4aU] = ((debug__DOT____Vlvbound_h92a32f7c__0[1U] 
                                                 >> 0x18U) 
                                                | (debug__DOT____Vlvbound_h92a32f7c__0[2U] 
                                                   << 8U));
    vlSelfRef.__PVT__debug__DOT__next[0x4bU] = ((0xc0000000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x4bU]) 
                                                | ((debug__DOT____Vlvbound_h92a32f7c__0[2U] 
                                                    >> 0x18U) 
                                                   | (debug__DOT____Vlvbound_h92a32f7c__0[3U] 
                                                      << 8U)));
    debug__DOT____Vlvbound_ha3a841b2__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0x44U] = ((0xfffc000fU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x44U]) 
                                                | ((IData)(debug__DOT____Vlvbound_ha3a841b2__0) 
                                                   << 4U));
    debug__DOT____Vlvbound_hf7de855a__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexIsReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x43U] = ((0xffc000ffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x43U]) 
                                                | ((IData)(debug__DOT____Vlvbound_hf7de855a__0) 
                                                   << 8U));
    debug__DOT____Vlvbound_hf74e2cd4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x42U] = ((0x3ffffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x42U]) 
                                                | ((IData)(debug__DOT____Vlvbound_hf74e2cd4__0) 
                                                   << 0x1aU));
    vlSelfRef.__PVT__debug__DOT__next[0x43U] = ((0xffffff00U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x43U]) 
                                                | ((IData)(debug__DOT____Vlvbound_hf74e2cd4__0) 
                                                   >> 6U));
    debug__DOT____Vlvbound_h4c346d8f__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
        [0U][0U];
    debug__DOT____Vlvbound_h4c346d8f__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
        [0U][1U];
    debug__DOT____Vlvbound_h4c346d8f__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
        [0U][2U];
    debug__DOT____Vlvbound_h4c346d8f__0[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
        [0U][3U];
    debug__DOT____Vlvbound_h4c346d8f__0[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexExReg
        [0U][4U];
    vlSelfRef.__PVT__debug__DOT__next[0x3eU] = ((0xffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x3eU]) 
                                                | (debug__DOT____Vlvbound_h4c346d8f__0[0U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x3fU] = ((debug__DOT____Vlvbound_h4c346d8f__0[0U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h4c346d8f__0[1U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x40U] = ((debug__DOT____Vlvbound_h4c346d8f__0[1U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h4c346d8f__0[2U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x41U] = ((debug__DOT____Vlvbound_h4c346d8f__0[2U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h4c346d8f__0[3U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x42U] = ((0xfc000000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x42U]) 
                                                | ((debug__DOT____Vlvbound_h4c346d8f__0[3U] 
                                                    >> 0x10U) 
                                                   | (debug__DOT____Vlvbound_h4c346d8f__0[4U] 
                                                      << 0x10U)));
    debug__DOT____Vlvbound_h907743c3__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x3eU] = ((0xffff0003U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x3eU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h907743c3__0) 
                                                   << 2U));
    debug__DOT____Vlvbound_h902d8323__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x3dU] = ((0xfff0003fU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x3dU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h902d8323__0) 
                                                   << 6U));
    debug__DOT____Vlvbound_h8fd8a9d3__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x3cU] = ((0xff0003ffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x3cU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h8fd8a9d3__0) 
                                                   << 0xaU));
    debug__DOT____Vlvbound_h64dfedfa__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
        [0U][0U];
    debug__DOT____Vlvbound_h64dfedfa__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
        [0U][1U];
    debug__DOT____Vlvbound_h64dfedfa__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
        [0U][2U];
    debug__DOT____Vlvbound_h64dfedfa__0[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
        [0U][3U];
    vlSelfRef.__PVT__debug__DOT__next[0x35U] = ((3U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x35U]) 
                                                | (debug__DOT____Vlvbound_h64dfedfa__0[0U] 
                                                   << 2U));
    vlSelfRef.__PVT__debug__DOT__next[0x36U] = ((debug__DOT____Vlvbound_h64dfedfa__0[0U] 
                                                 >> 0x1eU) 
                                                | (debug__DOT____Vlvbound_h64dfedfa__0[1U] 
                                                   << 2U));
    vlSelfRef.__PVT__debug__DOT__next[0x37U] = ((debug__DOT____Vlvbound_h64dfedfa__0[1U] 
                                                 >> 0x1eU) 
                                                | (debug__DOT____Vlvbound_h64dfedfa__0[2U] 
                                                   << 2U));
    vlSelfRef.__PVT__debug__DOT__next[0x38U] = ((0xffc00000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x38U]) 
                                                | ((debug__DOT____Vlvbound_h64dfedfa__0[2U] 
                                                    >> 0x1eU) 
                                                   | (debug__DOT____Vlvbound_h64dfedfa__0[3U] 
                                                      << 2U)));
    debug__DOT____Vlvbound_h05d1cf60__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [0U][0U];
    debug__DOT____Vlvbound_h05d1cf60__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [0U][1U];
    debug__DOT____Vlvbound_h05d1cf60__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [0U][2U];
    debug__DOT____Vlvbound_h05d1cf60__0[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [0U][3U];
    debug__DOT____Vlvbound_h05d1cf60__0[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [0U][4U];
    debug__DOT____Vlvbound_h05d1cf60__0[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [0U][5U];
    vlSelfRef.__PVT__debug__DOT__next[0x19U] = debug__DOT____Vlvbound_h05d1cf60__0[0U];
    vlSelfRef.__PVT__debug__DOT__next[0x1aU] = debug__DOT____Vlvbound_h05d1cf60__0[1U];
    vlSelfRef.__PVT__debug__DOT__next[0x1bU] = debug__DOT____Vlvbound_h05d1cf60__0[2U];
    vlSelfRef.__PVT__debug__DOT__next[0x1cU] = debug__DOT____Vlvbound_h05d1cf60__0[3U];
    vlSelfRef.__PVT__debug__DOT__next[0x1dU] = debug__DOT____Vlvbound_h05d1cf60__0[4U];
    vlSelfRef.__PVT__debug__DOT__next[0x1eU] = ((0xffff8000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x1eU]) 
                                                | debug__DOT____Vlvbound_h05d1cf60__0[5U]);
    debug__DOT____Vlvbound_h02afb21f__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [0U][0U];
    debug__DOT____Vlvbound_h02afb21f__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [0U][1U];
    debug__DOT____Vlvbound_h02afb21f__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [0U][2U];
    debug__DOT____Vlvbound_h02afb21f__0[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [0U][3U];
    debug__DOT____Vlvbound_h02afb21f__0[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [0U][4U];
    debug__DOT____Vlvbound_h02afb21f__0[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [0U][5U];
    debug__DOT____Vlvbound_h02afb21f__0[6U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [0U][6U];
    debug__DOT____Vlvbound_h02afb21f__0[7U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [0U][7U];
    debug__DOT____Vlvbound_h02afb21f__0[8U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [0U][8U];
    vlSelfRef.__PVT__debug__DOT__next[0x23U] = ((0x3fffffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x23U]) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[0U] 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x24U] = ((debug__DOT____Vlvbound_h02afb21f__0[0U] 
                                                 >> 2U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[1U] 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x25U] = ((debug__DOT____Vlvbound_h02afb21f__0[1U] 
                                                 >> 2U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[2U] 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x26U] = ((debug__DOT____Vlvbound_h02afb21f__0[2U] 
                                                 >> 2U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[3U] 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x27U] = ((debug__DOT____Vlvbound_h02afb21f__0[3U] 
                                                 >> 2U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[4U] 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x28U] = ((debug__DOT____Vlvbound_h02afb21f__0[4U] 
                                                 >> 2U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[5U] 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x29U] = ((debug__DOT____Vlvbound_h02afb21f__0[5U] 
                                                 >> 2U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[6U] 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x2aU] = ((debug__DOT____Vlvbound_h02afb21f__0[6U] 
                                                 >> 2U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[7U] 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x2bU] = ((debug__DOT____Vlvbound_h02afb21f__0[7U] 
                                                 >> 2U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[8U] 
                                                   << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x2cU] = ((0xffff0000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x2cU]) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[8U] 
                                                   >> 2U));
    debug__DOT____Vlvbound_h3dbf3c99__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x18U] = ((0xfffc000fU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x18U]) 
                                                | ((IData)(debug__DOT____Vlvbound_h3dbf3c99__0) 
                                                   << 4U));
    debug__DOT____Vlvbound_h902d8323__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memIsReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0x3dU] = ((0xfffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x3dU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h902d8323__0) 
                                                   << 0x14U));
    vlSelfRef.__PVT__debug__DOT__next[0x3eU] = ((0xfffffffcU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x3eU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h902d8323__0) 
                                                   >> 0xcU));
    debug__DOT____Vlvbound_h8fd8a9d3__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRrReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0x3cU] = ((0xffffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x3cU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h8fd8a9d3__0) 
                                                   << 0x18U));
    vlSelfRef.__PVT__debug__DOT__next[0x3dU] = ((0xffffffc0U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x3dU]) 
                                                | ((IData)(debug__DOT____Vlvbound_h8fd8a9d3__0) 
                                                   >> 8U));
    debug__DOT____Vlvbound_h64dfedfa__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
        [1U][0U];
    debug__DOT____Vlvbound_h64dfedfa__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
        [1U][1U];
    debug__DOT____Vlvbound_h64dfedfa__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
        [1U][2U];
    debug__DOT____Vlvbound_h64dfedfa__0[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memExReg
        [1U][3U];
    vlSelfRef.__PVT__debug__DOT__next[0x38U] = ((0x3fffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x38U]) 
                                                | (debug__DOT____Vlvbound_h64dfedfa__0[0U] 
                                                   << 0x16U));
    vlSelfRef.__PVT__debug__DOT__next[0x39U] = ((debug__DOT____Vlvbound_h64dfedfa__0[0U] 
                                                 >> 0xaU) 
                                                | (debug__DOT____Vlvbound_h64dfedfa__0[1U] 
                                                   << 0x16U));
    vlSelfRef.__PVT__debug__DOT__next[0x3aU] = ((debug__DOT____Vlvbound_h64dfedfa__0[1U] 
                                                 >> 0xaU) 
                                                | (debug__DOT____Vlvbound_h64dfedfa__0[2U] 
                                                   << 0x16U));
    vlSelfRef.__PVT__debug__DOT__next[0x3bU] = ((debug__DOT____Vlvbound_h64dfedfa__0[2U] 
                                                 >> 0xaU) 
                                                | (debug__DOT____Vlvbound_h64dfedfa__0[3U] 
                                                   << 0x16U));
    vlSelfRef.__PVT__debug__DOT__next[0x3cU] = ((0xfffffc00U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x3cU]) 
                                                | (debug__DOT____Vlvbound_h64dfedfa__0[3U] 
                                                   >> 0xaU));
    debug__DOT____Vlvbound_h05d1cf60__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [1U][0U];
    debug__DOT____Vlvbound_h05d1cf60__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [1U][1U];
    debug__DOT____Vlvbound_h05d1cf60__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [1U][2U];
    debug__DOT____Vlvbound_h05d1cf60__0[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [1U][3U];
    debug__DOT____Vlvbound_h05d1cf60__0[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [1U][4U];
    debug__DOT____Vlvbound_h05d1cf60__0[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__maReg
        [1U][5U];
    vlSelfRef.__PVT__debug__DOT__next[0x1eU] = ((0x7fffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x1eU]) 
                                                | (debug__DOT____Vlvbound_h05d1cf60__0[0U] 
                                                   << 0xfU));
    vlSelfRef.__PVT__debug__DOT__next[0x1fU] = ((debug__DOT____Vlvbound_h05d1cf60__0[0U] 
                                                 >> 0x11U) 
                                                | (debug__DOT____Vlvbound_h05d1cf60__0[1U] 
                                                   << 0xfU));
    vlSelfRef.__PVT__debug__DOT__next[0x20U] = ((debug__DOT____Vlvbound_h05d1cf60__0[1U] 
                                                 >> 0x11U) 
                                                | (debug__DOT____Vlvbound_h05d1cf60__0[2U] 
                                                   << 0xfU));
    vlSelfRef.__PVT__debug__DOT__next[0x21U] = ((debug__DOT____Vlvbound_h05d1cf60__0[2U] 
                                                 >> 0x11U) 
                                                | (debug__DOT____Vlvbound_h05d1cf60__0[3U] 
                                                   << 0xfU));
    vlSelfRef.__PVT__debug__DOT__next[0x22U] = ((debug__DOT____Vlvbound_h05d1cf60__0[3U] 
                                                 >> 0x11U) 
                                                | (debug__DOT____Vlvbound_h05d1cf60__0[4U] 
                                                   << 0xfU));
    vlSelfRef.__PVT__debug__DOT__next[0x23U] = ((0xc0000000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x23U]) 
                                                | ((debug__DOT____Vlvbound_h05d1cf60__0[4U] 
                                                    >> 0x11U) 
                                                   | (debug__DOT____Vlvbound_h05d1cf60__0[5U] 
                                                      << 0xfU)));
    debug__DOT____Vlvbound_h02afb21f__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [1U][0U];
    debug__DOT____Vlvbound_h02afb21f__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [1U][1U];
    debug__DOT____Vlvbound_h02afb21f__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [1U][2U];
    debug__DOT____Vlvbound_h02afb21f__0[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [1U][3U];
    debug__DOT____Vlvbound_h02afb21f__0[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [1U][4U];
    debug__DOT____Vlvbound_h02afb21f__0[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [1U][5U];
    debug__DOT____Vlvbound_h02afb21f__0[6U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [1U][6U];
    debug__DOT____Vlvbound_h02afb21f__0[7U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [1U][7U];
    debug__DOT____Vlvbound_h02afb21f__0[8U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__mtReg
        [1U][8U];
    vlSelfRef.__PVT__debug__DOT__next[0x2cU] = ((0xffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x2cU]) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[0U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x2dU] = ((debug__DOT____Vlvbound_h02afb21f__0[0U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[1U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x2eU] = ((debug__DOT____Vlvbound_h02afb21f__0[1U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[2U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x2fU] = ((debug__DOT____Vlvbound_h02afb21f__0[2U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[3U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x30U] = ((debug__DOT____Vlvbound_h02afb21f__0[3U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[4U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x31U] = ((debug__DOT____Vlvbound_h02afb21f__0[4U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[5U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x32U] = ((debug__DOT____Vlvbound_h02afb21f__0[5U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[6U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x33U] = ((debug__DOT____Vlvbound_h02afb21f__0[6U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[7U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x34U] = ((debug__DOT____Vlvbound_h02afb21f__0[7U] 
                                                 >> 0x10U) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[8U] 
                                                   << 0x10U));
    vlSelfRef.__PVT__debug__DOT__next[0x35U] = ((0xfffffffcU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x35U]) 
                                                | (debug__DOT____Vlvbound_h02afb21f__0[8U] 
                                                   >> 0x10U));
    debug__DOT____Vlvbound_h3dbf3c99__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0x18U] = ((0x3ffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x18U]) 
                                                | ((IData)(debug__DOT____Vlvbound_h3dbf3c99__0) 
                                                   << 0x12U));
    debug__DOT____Vlvbound_h376f222d__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpIsReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x17U] = ((0x3fffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x17U]) 
                                                | ((IData)(debug__DOT____Vlvbound_h376f222d__0) 
                                                   << 0x16U));
    vlSelfRef.__PVT__debug__DOT__next[0x18U] = ((0xfffffff0U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x18U]) 
                                                | ((IData)(debug__DOT____Vlvbound_h376f222d__0) 
                                                   >> 0xaU));
    debug__DOT____Vlvbound_h3785688d__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRrReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x17U] = ((0xffc000ffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x17U]) 
                                                | ((IData)(debug__DOT____Vlvbound_h3785688d__0) 
                                                   << 8U));
    debug__DOT____Vlvbound_h6455cb13__0[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
        [0U][0U];
    debug__DOT____Vlvbound_h6455cb13__0[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
        [0U][1U];
    debug__DOT____Vlvbound_h6455cb13__0[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
        [0U][2U];
    debug__DOT____Vlvbound_h6455cb13__0[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
        [0U][3U];
    debug__DOT____Vlvbound_h6455cb13__0[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
        [0U][4U];
    debug__DOT____Vlvbound_h6455cb13__0[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
        [0U][5U];
    debug__DOT____Vlvbound_h6455cb13__0[6U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpExReg
        [0U][6U];
    vlSelfRef.__PVT__debug__DOT__next[0x11U] = ((3U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x11U]) 
                                                | (debug__DOT____Vlvbound_h6455cb13__0[0U] 
                                                   << 2U));
    vlSelfRef.__PVT__debug__DOT__next[0x12U] = ((debug__DOT____Vlvbound_h6455cb13__0[0U] 
                                                 >> 0x1eU) 
                                                | (debug__DOT____Vlvbound_h6455cb13__0[1U] 
                                                   << 2U));
    vlSelfRef.__PVT__debug__DOT__next[0x13U] = ((debug__DOT____Vlvbound_h6455cb13__0[1U] 
                                                 >> 0x1eU) 
                                                | (debug__DOT____Vlvbound_h6455cb13__0[2U] 
                                                   << 2U));
    vlSelfRef.__PVT__debug__DOT__next[0x14U] = ((debug__DOT____Vlvbound_h6455cb13__0[2U] 
                                                 >> 0x1eU) 
                                                | (debug__DOT____Vlvbound_h6455cb13__0[3U] 
                                                   << 2U));
    vlSelfRef.__PVT__debug__DOT__next[0x15U] = ((debug__DOT____Vlvbound_h6455cb13__0[3U] 
                                                 >> 0x1eU) 
                                                | (debug__DOT____Vlvbound_h6455cb13__0[4U] 
                                                   << 2U));
    vlSelfRef.__PVT__debug__DOT__next[0x16U] = ((debug__DOT____Vlvbound_h6455cb13__0[4U] 
                                                 >> 0x1eU) 
                                                | (debug__DOT____Vlvbound_h6455cb13__0[5U] 
                                                   << 2U));
    vlSelfRef.__PVT__debug__DOT__next[0x17U] = ((0xffffff00U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x17U]) 
                                                | ((debug__DOT____Vlvbound_h6455cb13__0[5U] 
                                                    >> 0x1eU) 
                                                   | (debug__DOT____Vlvbound_h6455cb13__0[6U] 
                                                      << 2U)));
    debug__DOT____Vlvbound_h9954e88d__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0x10U] = ((0xfffffU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x10U]) 
                                                | ((IData)(debug__DOT____Vlvbound_h9954e88d__0) 
                                                   << 0x14U));
    vlSelfRef.__PVT__debug__DOT__next[0x11U] = ((0xfffffffcU 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x11U]) 
                                                | ((IData)(debug__DOT____Vlvbound_h9954e88d__0) 
                                                   >> 0xcU));
    debug__DOT____Vlvbound_h322a76c3__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0xfU] = ((0xc00000ffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xfU]) 
                                               | (debug__DOT____Vlvbound_h322a76c3__0 
                                                  << 8U));
    debug__DOT____Vlvbound_h322a76c3__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmReg
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0xfU] = ((0x3fffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xfU]) 
                                               | (debug__DOT____Vlvbound_h322a76c3__0 
                                                  << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0x10U] = ((0xfff00000U 
                                                 & vlSelfRef.__PVT__debug__DOT__next[0x10U]) 
                                                | (debug__DOT____Vlvbound_h322a76c3__0 
                                                   >> 2U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[0xeU] = ((0xfeffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xeU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 0x18U));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [0U];
    vlSelfRef.__PVT__debug__DOT__next[8U] = ((0xffe000ffU 
                                              & vlSelfRef.__PVT__debug__DOT__next[8U]) 
                                             | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                << 8U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[0xeU] = ((0xfdffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xeU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 0x19U));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [1U];
    vlSelfRef.__PVT__debug__DOT__next[8U] = ((0x1fffffU 
                                              & vlSelfRef.__PVT__debug__DOT__next[8U]) 
                                             | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                << 0x15U));
    vlSelfRef.__PVT__debug__DOT__next[9U] = ((0xfffffffcU 
                                              & vlSelfRef.__PVT__debug__DOT__next[9U]) 
                                             | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                >> 0xbU));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [2U];
    vlSelfRef.__PVT__debug__DOT__next[0xeU] = ((0xfbffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xeU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 0x1aU));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [2U];
    vlSelfRef.__PVT__debug__DOT__next[9U] = ((0xffff8003U 
                                              & vlSelfRef.__PVT__debug__DOT__next[9U]) 
                                             | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                << 2U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [3U];
    vlSelfRef.__PVT__debug__DOT__next[0xeU] = ((0xf7ffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xeU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 0x1bU));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [3U];
    vlSelfRef.__PVT__debug__DOT__next[9U] = ((0xf0007fffU 
                                              & vlSelfRef.__PVT__debug__DOT__next[9U]) 
                                             | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                << 0xfU));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [4U];
    vlSelfRef.__PVT__debug__DOT__next[0xeU] = ((0xefffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xeU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 0x1cU));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [4U];
    vlSelfRef.__PVT__debug__DOT__next[9U] = ((0xfffffffU 
                                              & vlSelfRef.__PVT__debug__DOT__next[9U]) 
                                             | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                << 0x1cU));
    vlSelfRef.__PVT__debug__DOT__next[0xaU] = ((0xfffffe00U 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xaU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  >> 4U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [5U];
    vlSelfRef.__PVT__debug__DOT__next[0xeU] = ((0xdfffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xeU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 0x1dU));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [5U];
    vlSelfRef.__PVT__debug__DOT__next[0xaU] = ((0xffc001ffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xaU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 9U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [6U];
    vlSelfRef.__PVT__debug__DOT__next[0xeU] = ((0xbfffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xeU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 0x1eU));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [6U];
    vlSelfRef.__PVT__debug__DOT__next[0xaU] = ((0x3fffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xaU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 0x16U));
    vlSelfRef.__PVT__debug__DOT__next[0xbU] = ((0xfffffff8U 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xbU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  >> 0xaU));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [7U];
    vlSelfRef.__PVT__debug__DOT__next[0xeU] = ((0x7fffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xeU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 0x1fU));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [7U];
    vlSelfRef.__PVT__debug__DOT__next[0xbU] = ((0xffff0007U 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xbU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 3U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [8U];
    vlSelfRef.__PVT__debug__DOT__next[0xfU] = ((0xfffffffeU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xfU]) 
                                               | (IData)(debug__DOT____Vlvbound_h4ff58c65__0));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [8U];
    vlSelfRef.__PVT__debug__DOT__next[0xbU] = ((0xe000ffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xbU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 0x10U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [9U];
    vlSelfRef.__PVT__debug__DOT__next[0xfU] = ((0xfffffffdU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xfU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 1U));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [9U];
    vlSelfRef.__PVT__debug__DOT__next[0xbU] = ((0x1fffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xbU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 0x1dU));
    vlSelfRef.__PVT__debug__DOT__next[0xcU] = ((0xfffffc00U 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xcU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  >> 3U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [0xaU];
    vlSelfRef.__PVT__debug__DOT__next[0xfU] = ((0xfffffffbU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xfU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 2U));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [0xaU];
    vlSelfRef.__PVT__debug__DOT__next[0xcU] = ((0xff8003ffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xcU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 0xaU));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [0xbU];
    vlSelfRef.__PVT__debug__DOT__next[0xfU] = ((0xfffffff7U 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xfU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 3U));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [0xbU];
    vlSelfRef.__PVT__debug__DOT__next[0xcU] = ((0x7fffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xcU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 0x17U));
    vlSelfRef.__PVT__debug__DOT__next[0xdU] = ((0xfffffff0U 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xdU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  >> 9U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [0xcU];
    vlSelfRef.__PVT__debug__DOT__next[0xfU] = ((0xffffffefU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xfU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 4U));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [0xcU];
    vlSelfRef.__PVT__debug__DOT__next[0xdU] = ((0xfffe000fU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xdU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 4U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [0xdU];
    vlSelfRef.__PVT__debug__DOT__next[0xfU] = ((0xffffffdfU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xfU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 5U));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [0xdU];
    vlSelfRef.__PVT__debug__DOT__next[0xdU] = ((0xc001ffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xdU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 0x11U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [0xeU];
    vlSelfRef.__PVT__debug__DOT__next[0xfU] = ((0xffffffbfU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xfU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 6U));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [0xeU];
    vlSelfRef.__PVT__debug__DOT__next[0xdU] = ((0x3fffffffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xdU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 0x1eU));
    vlSelfRef.__PVT__debug__DOT__next[0xeU] = ((0xfffff800U 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xeU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  >> 2U));
    debug__DOT____Vlvbound_h4ff58c65__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__scheduler
        [0xfU];
    vlSelfRef.__PVT__debug__DOT__next[0xfU] = ((0xffffff7fU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xfU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h4ff58c65__0) 
                                                  << 7U));
    debug__DOT____Vlvbound_h419daeb4__0 = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__issueQueue
        [0xfU];
    vlSelfRef.__PVT__debug__DOT__next[0xeU] = ((0xff0007ffU 
                                                & vlSelfRef.__PVT__debug__DOT__next[0xeU]) 
                                               | ((IData)(debug__DOT____Vlvbound_h419daeb4__0) 
                                                  << 0xbU));
    vlSelfRef.__PVT__debug__DOT__next[7U] = ((0xffffU 
                                              & vlSelfRef.__PVT__debug__DOT__next[7U]) 
                                             | (0xffff0000U 
                                                & ((((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount) 
                                                     << 0x1aU) 
                                                    | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npStagePipeCtrl) 
                                                       << 0x18U)) 
                                                   | ((((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifStagePipeCtrl) 
                                                        << 0x16U) 
                                                       | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__pdStagePipeCtrl) 
                                                          << 0x14U)) 
                                                      | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idStagePipeCtrl) 
                                                          << 0x12U) 
                                                         | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnStagePipeCtrl) 
                                                            << 0x10U))))));
    vlSelfRef.__PVT__debug__DOT__next[8U] = ((0xffffff00U 
                                              & vlSelfRef.__PVT__debug__DOT__next[8U]) 
                                             | (0xffffU 
                                                & ((((0xff80U 
                                                      & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__toRecoveryPhase) 
                                                         << 7U)) 
                                                     | (0xfffeU 
                                                        & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.headPtr) 
                                                           << 1U))) 
                                                    | (0xffffU 
                                                       & (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount) 
                                                           >> 6U) 
                                                          | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__npStagePipeCtrl) 
                                                             >> 8U)))) 
                                                   | ((0xffffU 
                                                       & (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__ifStagePipeCtrl) 
                                                           >> 0xaU) 
                                                          | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__pdStagePipeCtrl) 
                                                             >> 0xcU))) 
                                                      | ((0xffffU 
                                                          & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__idStagePipeCtrl) 
                                                             >> 0xeU)) 
                                                         | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnStagePipeCtrl) 
                                                            >> 0x10U))))));
    vlSelfRef.__PVT__debug__DOT__next[7U] = ((0xffff0001U 
                                              & vlSelfRef.__PVT__debug__DOT__next[7U]) 
                                             | (0xfffffffeU 
                                                & (((((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__dsStagePipeCtrl) 
                                                      << 0xeU) 
                                                     | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__backEndPipeCtrl) 
                                                        << 0xcU)) 
                                                    | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmStagePipeCtrl) 
                                                        << 0xaU) 
                                                       | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__stallByDecodeStage) 
                                                          << 9U))) 
                                                   | ((((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatable) 
                                                        << 8U) 
                                                       | ((IData)(vlSelfRef.__PVT__storeCommitter__DOT__phase) 
                                                          << 7U)) 
                                                      | (((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount) 
                                                          << 2U) 
                                                         | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__busyInRecovery) 
                                                            << 1U))))));
    vlSelfRef.__PVT__debug__DOT__next[0U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[0U];
    vlSelfRef.__PVT__debug__DOT__next[1U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[1U];
    vlSelfRef.__PVT__debug__DOT__next[2U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[2U];
    vlSelfRef.__PVT__debug__DOT__next[3U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[3U];
    vlSelfRef.__PVT__debug__DOT__next[4U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[4U];
    vlSelfRef.__PVT__debug__DOT__next[5U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[5U];
    vlSelfRef.__PVT__debug__DOT__next[6U] = vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__perfCounter[6U];
    vlSelfRef.__PVT__debug__DOT__next[7U] = ((0xfffffffeU 
                                              & vlSelfRef.__PVT__debug__DOT__next[7U]) 
                                             | (0U 
                                                == (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount)));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___ico_comb__TOP__SMT_RTL_Testbench__core__0(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___ico_comb__TOP__SMT_RTL_Testbench__core__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__recoveryManager__DOT__toRecoveryPhase 
        = ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage) 
           | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInRwStage));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U] 
        = ((0xfffffff7U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U]) 
           | (((IData)(vlSelfRef.__PVT__recoveryManager__DOT__toRecoveryPhase) 
               && (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInRwStage)) 
              << 3U));
    vlSelfRef.__PVT__recoveryManager__DOT__toCommitPhase 
        = (IData)(((0x400000U == (0x600000U & vlSelfRef.__PVT__recoveryManager__DOT__regState[3U])) 
                   & (~ ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__renameLogicRecoveryRMT) 
                         | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__issueQueueReturnIndex)))));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U] 
        = ((0xfffffff8U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U]) 
           | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage)
               ? (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__refetchTypeFromCommitStage)
               : (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__refetchTypeFromRwStage)));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U] 
        = ((0xffffU & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U]) 
           | ((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage)) 
                        << 0x20U) | (QData)((IData)(
                                                    ((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                                      << 0xdU) 
                                                     | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                        >> 0x13U)))))) 
              << 0x10U));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[1U] 
        = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage)) 
                      << 0x20U) | (QData)((IData)((
                                                   (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                                    << 0xdU) 
                                                   | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                      >> 0x13U)))))) 
            >> 0x10U) | ((vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromCommitStage 
                          << 0x14U) | ((IData)(((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage)) 
                                                  << 0x20U) 
                                                 | (QData)((IData)(
                                                                   ((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                                                     << 0xdU) 
                                                                    | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                                       >> 0x13U))))) 
                                                >> 0x20U)) 
                                       << 0x10U)));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[2U] 
        = ((0xfff00000U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[2U]) 
           | (((0xffffU & (vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromCommitStage 
                           >> 0xcU)) | ((IData)(((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveryCauseFromCommitStage)) 
                                                   << 0x20U) 
                                                  | (QData)((IData)(
                                                                    ((vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[1U] 
                                                                      << 0xdU) 
                                                                     | (vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryReg[0U] 
                                                                        >> 0x13U))))) 
                                                 >> 0x20U)) 
                                        >> 0x10U)) 
              | (0xf0000U & (vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromCommitStage 
                             >> 0xcU))));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[2U] 
        = ((0xfffffU & vlSelfRef.__PVT__recoveryManager__DOT__nextState[2U]) 
           | ((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage)) 
                        << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromRwStage)))) 
              << 0x14U));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[3U] 
        = ((0x600000U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[3U]) 
           | (0x7fffffU & (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage)) 
                                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromRwStage)))) 
                            >> 0xcU) | ((IData)(((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__exceptionDetectedInCommitStage)) 
                                                   << 0x20U) 
                                                  | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__recoveredPC_FromRwStage))) 
                                                 >> 0x20U)) 
                                        << 0x14U))));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[3U] 
        = ((0x1fffffU & vlSelfRef.__PVT__recoveryManager__DOT__nextState[3U]) 
           | (0x7fffffU & (((IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)
                             ? 0U : (3U & ((0U == (3U 
                                                   & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                      >> 0x15U)))
                                            ? ((IData)(vlSelfRef.__PVT__recoveryManager__DOT__toRecoveryPhase)
                                                ? 1U
                                                : 0U)
                                            : ((1U 
                                                == 
                                                (3U 
                                                 & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                    >> 0x15U)))
                                                ? 2U
                                                : ((IData)(vlSelfRef.__PVT__recoveryManager__DOT__toCommitPhase)
                                                    ? 0U
                                                    : 
                                                   ((vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                     << 0xbU) 
                                                    | (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                       >> 0x15U))))))) 
                           << 0x15U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__toCommitPhase 
        = vlSelfRef.__PVT__recoveryManager__DOT__toCommitPhase;
    vlSelfRef.__PVT__recoveryManager__DOT__exceptionOpPtr 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__exceptionOpPtr;
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U] 
        = ((0xffff03ffU & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U]) 
           | (0xfc00U & ((((0U == (7U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U])) 
                           | (5U == (7U & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U])))
                           ? (IData)(vlSelfRef.__PVT__recoveryManager__DOT__exceptionOpPtr)
                           : ((IData)(1U) + (IData)(vlSelfRef.__PVT__recoveryManager__DOT__exceptionOpPtr))) 
                         << 0xaU)));
    vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U] 
        = ((0xfffffc0fU & vlSelfRef.__PVT__recoveryManager__DOT__nextState[0U]) 
           | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__detectedFlushRangeTailPtr) 
              << 4U));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__0(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlWide<5>/*138:0*/ complexRwStage__DOT____Vlvbound_hfc50245b__0;
    VL_ZERO_W(139, complexRwStage__DOT____Vlvbound_hfc50245b__0);
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h0285a07a__0;
    complexRwStage__DOT____Vlvbound_h0285a07a__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h144dd5b6__0;
    complexRwStage__DOT____Vlvbound_h144dd5b6__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h046b483c__0;
    complexRwStage__DOT____Vlvbound_h046b483c__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h8546e2b5__0;
    complexRwStage__DOT____Vlvbound_h8546e2b5__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h1619d9fe__0;
    complexRwStage__DOT____Vlvbound_h1619d9fe__0 = 0;
    CData/*6:0*/ complexRwStage__DOT____Vlvbound_h9f79b102__0;
    complexRwStage__DOT____Vlvbound_h9f79b102__0 = 0;
    QData/*32:0*/ complexRwStage__DOT____Vlvbound_h7efdbe27__0;
    complexRwStage__DOT____Vlvbound_h7efdbe27__0 = 0;
    CData/*5:0*/ complexRwStage__DOT____Vlvbound_h8cf597d3__0;
    complexRwStage__DOT____Vlvbound_h8cf597d3__0 = 0;
    CData/*3:0*/ complexRwStage__DOT____Vlvbound_h851d8249__0;
    complexRwStage__DOT____Vlvbound_h851d8249__0 = 0;
    CData/*3:0*/ complexRwStage__DOT____Vlvbound_h851db172__0;
    complexRwStage__DOT____Vlvbound_h851db172__0 = 0;
    CData/*5:0*/ complexRwStage__DOT____Vlvbound_h8cf597d3__1;
    complexRwStage__DOT____Vlvbound_h8cf597d3__1 = 0;
    IData/*19:0*/ complexRwStage__DOT____Vlvbound_h7f5b6f62__0;
    complexRwStage__DOT____Vlvbound_h7f5b6f62__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h32678e71__0;
    complexRwStage__DOT____Vlvbound_h32678e71__0 = 0;
    VlWide<3>/*71:0*/ complexRwStage__DOT____Vlvbound_haccc5680__0;
    VL_ZERO_W(72, complexRwStage__DOT____Vlvbound_haccc5680__0);
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h59c1814f__0;
    complexRwStage__DOT____Vlvbound_h59c1814f__0 = 0;
    CData/*0:0*/ complexRwStage__DOT____Vlvbound_h59ed0fc0__0;
    complexRwStage__DOT____Vlvbound_h59ed0fc0__0 = 0;
    SData/*11:0*/ complexRwStage__DOT____Vlvbound_h8bf3b354__0;
    complexRwStage__DOT____Vlvbound_h8bf3b354__0 = 0;
    CData/*0:0*/ memRwStage__DOT____Vlvbound_hb4b12956__0;
    memRwStage__DOT____Vlvbound_hb4b12956__0 = 0;
    CData/*6:0*/ memRwStage__DOT____Vlvbound_hca7847bb__0;
    memRwStage__DOT____Vlvbound_hca7847bb__0 = 0;
    QData/*32:0*/ memRwStage__DOT____Vlvbound_h6f0537ce__0;
    memRwStage__DOT____Vlvbound_h6f0537ce__0 = 0;
    VlWide<3>/*92:0*/ fpRwStage__DOT____Vlvbound_hb611356e__0;
    VL_ZERO_W(93, fpRwStage__DOT____Vlvbound_hb611356e__0);
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h0285a07a__0;
    fpRwStage__DOT____Vlvbound_h0285a07a__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h144dd5b6__0;
    fpRwStage__DOT____Vlvbound_h144dd5b6__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h046b483c__0;
    fpRwStage__DOT____Vlvbound_h046b483c__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h8546e2b5__0;
    fpRwStage__DOT____Vlvbound_h8546e2b5__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h21fb6822__0;
    fpRwStage__DOT____Vlvbound_h21fb6822__0 = 0;
    CData/*6:0*/ fpRwStage__DOT____Vlvbound_h6f02ee4c__0;
    fpRwStage__DOT____Vlvbound_h6f02ee4c__0 = 0;
    QData/*32:0*/ fpRwStage__DOT____Vlvbound_he6dc3282__0;
    fpRwStage__DOT____Vlvbound_he6dc3282__0 = 0;
    CData/*5:0*/ fpRwStage__DOT____Vlvbound_h8cf597d3__0;
    fpRwStage__DOT____Vlvbound_h8cf597d3__0 = 0;
    CData/*3:0*/ fpRwStage__DOT____Vlvbound_h851d8249__0;
    fpRwStage__DOT____Vlvbound_h851d8249__0 = 0;
    CData/*3:0*/ fpRwStage__DOT____Vlvbound_h851db172__0;
    fpRwStage__DOT____Vlvbound_h851db172__0 = 0;
    IData/*19:0*/ fpRwStage__DOT____Vlvbound_h7f5b6f62__0;
    fpRwStage__DOT____Vlvbound_h7f5b6f62__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_he995c148__0;
    fpRwStage__DOT____Vlvbound_he995c148__0 = 0;
    VlWide<3>/*71:0*/ fpRwStage__DOT____Vlvbound_h8af695fc__0;
    VL_ZERO_W(72, fpRwStage__DOT____Vlvbound_h8af695fc__0);
    CData/*4:0*/ fpRwStage__DOT____Vlvbound_h1d55e97c__0;
    fpRwStage__DOT____Vlvbound_h1d55e97c__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h48155c9e__0;
    fpRwStage__DOT____Vlvbound_h48155c9e__0 = 0;
    CData/*0:0*/ fpRwStage__DOT____Vlvbound_h48154c0c__0;
    fpRwStage__DOT____Vlvbound_h48154c0c__0 = 0;
    SData/*11:0*/ fpRwStage__DOT____Vlvbound_h5e9ebd83__0;
    fpRwStage__DOT____Vlvbound_h5e9ebd83__0 = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__548__detectRange;
    __Vfunc_SelectiveFlushDetector__548__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__548__headPtr;
    __Vfunc_SelectiveFlushDetector__548__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__548__tailPtr;
    __Vfunc_SelectiveFlushDetector__548__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__548__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__548__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__548__opPtr;
    __Vfunc_SelectiveFlushDetector__548__opPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__554__detectRange;
    __Vfunc_SelectiveFlushDetector__554__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__554__headPtr;
    __Vfunc_SelectiveFlushDetector__554__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__554__tailPtr;
    __Vfunc_SelectiveFlushDetector__554__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__554__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__554__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__554__opPtr;
    __Vfunc_SelectiveFlushDetector__554__opPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__618__detectRange;
    __Vfunc_SelectiveFlushDetector__618__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__618__headPtr;
    __Vfunc_SelectiveFlushDetector__618__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__618__tailPtr;
    __Vfunc_SelectiveFlushDetector__618__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__618__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__618__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__618__opPtr;
    __Vfunc_SelectiveFlushDetector__618__opPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__630__detectRange;
    __Vfunc_SelectiveFlushDetector__630__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__630__headPtr;
    __Vfunc_SelectiveFlushDetector__630__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__630__tailPtr;
    __Vfunc_SelectiveFlushDetector__630__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__630__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__630__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__630__opPtr;
    __Vfunc_SelectiveFlushDetector__630__opPtr = 0;
    // Body
    if (vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst) {
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk17__DOT__i = 8U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk19__DOT__way = 2U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk14__DOT__p = 2U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk20__DOT__way = 2U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk14__DOT__unnamedblk15__DOT__way = 2U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk21__DOT__i = 1U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk14__DOT__unnamedblk15__DOT__unnamedblk16__DOT__i = 8U;
        vlSelfRef.__PVT__dCache__DOT__array__DOT__unnamedblk17__DOT__unnamedblk18__DOT__way = 2U;
        vlSelfRef.__PVT__renameLogicCommitter__DOT__nextPhase = 0U;
        vlSelfRef.__PVT__renameLogicCommitter__DOT__nextRecoveryCount = 0U;
        vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__array__rst 
            = vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst;
        vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__array__rst 
            = vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst;
        vlSelfRef.iCache__DOT____Vcellinp__nruStateArray__rst 
            = vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst;
        vlSelfRef.__PVT__controller__DOT__cmStage = 1U;
        vlSelfRef.__PVT__controller__DOT__dsStage = 1U;
        vlSelfRef.__PVT__controller__DOT__backEnd = 1U;
    } else {
        vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__array__rst 
            = vlSelfRef.__PVT__iCache__DOT__regFlush;
        vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__array__rst 
            = vlSelfRef.__PVT__iCache__DOT__regFlush;
        vlSelfRef.iCache__DOT____Vcellinp__nruStateArray__rst 
            = vlSelfRef.__PVT__iCache__DOT__regFlush;
        vlSelfRef.__PVT__controller__DOT__cmStage = 0U;
        vlSelfRef.__PVT__controller__DOT__dsStage = 0U;
        if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                          >> 0x15U)))) {
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextPhase = 1U;
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextRecoveryCount = 0U;
            vlSelfRef.__PVT__controller__DOT__dsStage = 1U;
        } else if ((1U == (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__phase))) {
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextPhase = 2U;
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextRecoveryCount 
                = (0x7fU & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__recoveryEntryNum));
        } else if ((2U == (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__phase))) {
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextPhase 
                = ((0U == (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__recoveryCount))
                    ? 0U : 2U);
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextRecoveryCount 
                = (0x7fU & ((2U < (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__recoveryCount))
                             ? ((IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__recoveryCount) 
                                - (IData)(2U)) : 0U));
        } else {
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextPhase 
                = vlSelfRef.__PVT__renameLogicCommitter__DOT__phase;
            vlSelfRef.__PVT__renameLogicCommitter__DOT__nextRecoveryCount 
                = (0x7fU & (IData)(vlSelfRef.__PVT__renameLogicCommitter__DOT__recoveryCount));
        }
        vlSelfRef.__PVT__controller__DOT__backEnd = 0U;
    }
    vlSelfRef.__PVT__storeCommitter__DOT__nextPhase 
        = ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
           && ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                             >> 0x15U))) || ((1U & 
                                              (~ (IData)(vlSelfRef.__PVT__storeCommitter__DOT__phase))) 
                                             && (IData)(vlSelfRef.__PVT__storeCommitter__DOT__phase))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__cmStage 
        = vlSelfRef.__PVT__controller__DOT__cmStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__cmStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__cmStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__dsStage 
        = vlSelfRef.__PVT__controller__DOT__dsStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__dsStagePipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__dsStage;
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd 
        = vlSelfRef.__PVT__controller__DOT__backEnd;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__backEndPipeCtrl 
        = vlSelfRef.__PVT__controller__DOT__backEnd;
    vlSelfRef.__PVT__replayQueue__DOT__mshrNotReady 
        = ((2U & (IData)(vlSelfRef.__PVT__replayQueue__DOT__mshrNotReady)) 
           | ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
              && (0x11U > vlSelfRef.__PVT__replayQueue__DOT__mshrPhase
                  [vlSelfRef.__PVT__replayQueue__DOT__mshrID
                  [0U]])));
    vlSelfRef.__PVT__replayQueue__DOT__mshrNotReady 
        = ((1U & (IData)(vlSelfRef.__PVT__replayQueue__DOT__mshrNotReady)) 
           | (((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
               && (0x11U > vlSelfRef.__PVT__replayQueue__DOT__mshrPhase
                   [vlSelfRef.__PVT__replayQueue__DOT__mshrID
                   [1U]])) << 1U));
    vlSelfRef.__PVT__replayQueue__DOT__targetMSHRValid 
        = ((2U & (IData)(vlSelfRef.__PVT__replayQueue__DOT__targetMSHRValid)) 
           | ((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
              && vlSelfRef.__PVT__replayQueue__DOT__mshrValid
              [vlSelfRef.__PVT__replayQueue__DOT__mshrID
              [0U]]));
    vlSelfRef.__PVT__replayQueue__DOT__targetMSHRValid 
        = ((1U & (IData)(vlSelfRef.__PVT__replayQueue__DOT__targetMSHRValid)) 
           | (((1U & (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst))) 
               && vlSelfRef.__PVT__replayQueue__DOT__mshrValid
               [vlSelfRef.__PVT__replayQueue__DOT__mshrID
               [1U]]) << 1U));
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__writeWayData[0U] 
        = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U])))) 
            << 0xcU) | ((0x800U & ((~ (IData)(vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__0__KET____DOT__array__rst)) 
                                   << 0xbU)) | (IData)(vlSelfRef.__PVT__iCache__DOT__regMissTag)));
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__writeWayData[1U] 
        = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U])))) 
            >> 0x14U) | ((IData)(((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U]))) 
                                  >> 0x20U)) << 0xcU));
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__0__KET____DOT__array__DOT__writeWayData[2U] 
        = ((IData)(((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U]))) 
                    >> 0x20U)) >> 0x14U);
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__writeWayData[0U] 
        = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U])))) 
            << 0xcU) | ((0x800U & ((~ (IData)(vlSelfRef.iCache__DOT____Vcellinp__genblk1__BRA__1__KET____DOT__array__rst)) 
                                   << 0xbU)) | (IData)(vlSelfRef.__PVT__iCache__DOT__regMissTag)));
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__writeWayData[1U] 
        = (((IData)((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U])))) 
            >> 0x14U) | ((IData)(((((QData)((IData)(
                                                    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U]))) 
                                  >> 0x20U)) << 0xcU));
    vlSelfRef.__PVT__iCache__DOT__genblk1__BRA__1__KET____DOT__array__DOT__writeWayData[2U] 
        = ((IData)(((((QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[1U])) 
                      << 0x20U) | (QData)((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__icMemAccessResult[0U]))) 
                    >> 0x20U)) >> 0x14U);
    vlSelfRef.__PVT__dsStage__DOT__stall = (1U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__dsStage) 
                                                  >> 1U));
    vlSelfRef.__PVT__dsStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__dsStage));
    vlSelfRef.__PVT__dsStage__DOT__update[0U] = (1U 
                                                 & (((~ (IData)(vlSelfRef.__PVT__dsStage__DOT__stall)) 
                                                     & (~ (IData)(vlSelfRef.__PVT__dsStage__DOT__clear))) 
                                                    & (vlSelfRef.__PVT__dsStage__DOT__pipeReg
                                                       [0U][6U] 
                                                       >> 0xaU)));
    vlSelfRef.__PVT__dsStage__DOT__update[1U] = (1U 
                                                 & (((~ (IData)(vlSelfRef.__PVT__dsStage__DOT__stall)) 
                                                     & (~ (IData)(vlSelfRef.__PVT__dsStage__DOT__clear))) 
                                                    & (vlSelfRef.__PVT__dsStage__DOT__pipeReg
                                                       [1U][6U] 
                                                       >> 0xaU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write[0U] 
        = vlSelfRef.__PVT__dsStage__DOT__update[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write[1U] 
        = vlSelfRef.__PVT__dsStage__DOT__update[1U];
    vlSelfRef.__PVT__complexRwStage__DOT__stall = (1U 
                                                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                      >> 1U));
    vlSelfRef.__PVT__complexRwStage__DOT__clear = (1U 
                                                   & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    complexRwStage__DOT____Vlvbound_hfc50245b__0[0U] 
        = ((vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
            [0U][2U] << 0x1fU) | (vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
                                  [0U][1U] >> 1U));
    complexRwStage__DOT____Vlvbound_hfc50245b__0[1U] 
        = ((vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
            [0U][3U] << 0x1fU) | (vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
                                  [0U][2U] >> 1U));
    complexRwStage__DOT____Vlvbound_hfc50245b__0[2U] 
        = (0x3ffffU & (vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
                       [0U][3U] >> 1U));
    complexRwStage__DOT____Vlvbound_hfc50245b__0[3U] = 0U;
    complexRwStage__DOT____Vlvbound_hfc50245b__0[4U] = 0U;
    vlSelfRef.__PVT__complexRwStage__DOT__iqData[0U][0U] 
        = complexRwStage__DOT____Vlvbound_hfc50245b__0[0U];
    vlSelfRef.__PVT__complexRwStage__DOT__iqData[0U][1U] 
        = complexRwStage__DOT____Vlvbound_hfc50245b__0[1U];
    vlSelfRef.__PVT__complexRwStage__DOT__iqData[0U][2U] 
        = complexRwStage__DOT____Vlvbound_hfc50245b__0[2U];
    vlSelfRef.__PVT__complexRwStage__DOT__iqData[0U][3U] 
        = complexRwStage__DOT____Vlvbound_hfc50245b__0[3U];
    vlSelfRef.__PVT__complexRwStage__DOT__iqData[0U][4U] 
        = complexRwStage__DOT____Vlvbound_hfc50245b__0[4U];
    complexRwStage__DOT____Vlvbound_h0285a07a__0 = 
        (1U & vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
         [0U][1U]);
    vlSelfRef.__PVT__complexRwStage__DOT__regValid[0U] 
        = complexRwStage__DOT____Vlvbound_h0285a07a__0;
    complexRwStage__DOT____Vlvbound_h144dd5b6__0 = 
        (1U & (vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
               [0U][3U] >> 0x13U));
    vlSelfRef.__PVT__complexRwStage__DOT__valid[0U] 
        = complexRwStage__DOT____Vlvbound_h144dd5b6__0;
    __Vfunc_SelectiveFlushDetector__554__opPtr = (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__554__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__554__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__554__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__554__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__554__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__554__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 1U;
                goto __Vlabel1;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__554__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__554__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__554__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__554__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__554__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 1U;
                    goto __Vlabel1;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 0U;
                    goto __Vlabel1;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__554__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__554__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__554__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__554__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__554__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 1U;
                    goto __Vlabel1;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__554__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__554__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__554__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 1U;
                    goto __Vlabel1;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 0U;
                    goto __Vlabel1;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 0U;
                goto __Vlabel1;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout = 0U;
        }
        __Vlabel1: ;
    }
    complexRwStage__DOT____Vlvbound_h046b483c__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__554__Vfuncout;
    vlSelfRef.__PVT__complexRwStage__DOT__flush[0U] 
        = complexRwStage__DOT____Vlvbound_h046b483c__0;
    complexRwStage__DOT____Vlvbound_h8546e2b5__0 = 
        ((((~ (IData)(vlSelfRef.__PVT__complexRwStage__DOT__stall)) 
           & (~ (IData)(vlSelfRef.__PVT__complexRwStage__DOT__clear))) 
          & vlSelfRef.__PVT__complexRwStage__DOT__valid
          [0U]) & (~ vlSelfRef.__PVT__complexRwStage__DOT__flush
                   [0U]));
    vlSelfRef.__PVT__complexRwStage__DOT__update[0U] 
        = complexRwStage__DOT____Vlvbound_h8546e2b5__0;
    complexRwStage__DOT____Vlvbound_h1619d9fe__0 = 
        (vlSelfRef.__PVT__complexRwStage__DOT__update
         [0U] & (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                 [0U][0U] >> 0x1cU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegWE[0U] 
        = complexRwStage__DOT____Vlvbound_h1619d9fe__0;
    complexRwStage__DOT____Vlvbound_h9f79b102__0 = 
        (0x7fU & (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                  [0U][0U] >> 0x15U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegNum[0U] 
        = complexRwStage__DOT____Vlvbound_h9f79b102__0;
    complexRwStage__DOT____Vlvbound_h7efdbe27__0 = 
        (0x1ffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
                                            [0U][1U])) 
                            << 0x20U) | (QData)((IData)(
                                                        vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
                                                        [0U][0U]))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexDstRegData[0U] 
        = complexRwStage__DOT____Vlvbound_h7efdbe27__0;
    complexRwStage__DOT____Vlvbound_h8cf597d3__0 = 
        (vlSelfRef.__PVT__complexRwStage__DOT__iqData
         [0U][1U] >> 0x1aU);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][2U] 
        = ((3U & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][2U]) | (0xffU & ((IData)(complexRwStage__DOT____Vlvbound_h8cf597d3__0) 
                                  << 2U)));
    complexRwStage__DOT____Vlvbound_h851d8249__0 = 
        (0xfU & (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                 [0U][1U] >> 0x16U));
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][1U]) | ((IData)(complexRwStage__DOT____Vlvbound_h851d8249__0) 
                         << 0x1eU));
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][2U]) | (0xffU & ((IData)(complexRwStage__DOT____Vlvbound_h851d8249__0) 
                                  >> 2U)));
    complexRwStage__DOT____Vlvbound_h851db172__0 = 
        (0xfU & (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                 [0U][1U] >> 0x12U));
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][1U]) | ((IData)(complexRwStage__DOT____Vlvbound_h851db172__0) 
                         << 0x1aU));
    complexRwStage__DOT____Vlvbound_h8cf597d3__1 = 
        (vlSelfRef.__PVT__complexRwStage__DOT__iqData
         [0U][1U] >> 0x1aU);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][2U] 
        = ((3U & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][2U]) | (0xffU & ((IData)(complexRwStage__DOT____Vlvbound_h8cf597d3__1) 
                                  << 2U)));
    complexRwStage__DOT____Vlvbound_h7f5b6f62__0 = 
        (0xfffffU & (vlSelfRef.__PVT__complexRwStage__DOT__iqData
                     [0U][0U] >> 1U));
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][1U]) | (complexRwStage__DOT____Vlvbound_h7f5b6f62__0 
                         << 2U));
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][0U] 
        = (3U & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][1U] 
        = (0xfffffffcU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
           [0U][1U]);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffdU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffeU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__complexRwStage__DOT__alWriteData[0U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
            [0U][1U]) | (((vlSelfRef.__PVT__complexRwStage__DOT__update
                           [0U] & vlSelfRef.__PVT__complexRwStage__DOT__regValid
                           [0U]) ? 1U : 0U) << 0x16U));
    complexRwStage__DOT____Vlvbound_h32678e71__0 = 
        vlSelfRef.__PVT__complexRwStage__DOT__update
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWrite[0U] 
        = complexRwStage__DOT____Vlvbound_h32678e71__0;
    complexRwStage__DOT____Vlvbound_haccc5680__0[0U] 
        = vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
        [0U][0U];
    complexRwStage__DOT____Vlvbound_haccc5680__0[1U] 
        = vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
        [0U][1U];
    complexRwStage__DOT____Vlvbound_haccc5680__0[2U] 
        = vlSelfRef.__PVT__complexRwStage__DOT__alWriteData
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData[0U][0U] 
        = complexRwStage__DOT____Vlvbound_haccc5680__0[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData[0U][1U] 
        = complexRwStage__DOT____Vlvbound_haccc5680__0[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__complexWriteData[0U][2U] 
        = complexRwStage__DOT____Vlvbound_haccc5680__0[2U];
    vlSelfRef.__PVT__complexRwStage__DOT__unnamedblk3__DOT__i = 1U;
    complexRwStage__DOT____Vlvbound_h59c1814f__0 = 
        vlSelfRef.__PVT__complexRwStage__DOT__valid
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg
            [0U]) | ((IData)(complexRwStage__DOT____Vlvbound_h59c1814f__0) 
                     << 0xdU));
    complexRwStage__DOT____Vlvbound_h59ed0fc0__0 = 
        vlSelfRef.__PVT__complexRwStage__DOT__flush
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg
            [0U]) | ((IData)(complexRwStage__DOT____Vlvbound_h59ed0fc0__0) 
                     << 0xcU));
    complexRwStage__DOT____Vlvbound_h8bf3b354__0 = 
        (vlSelfRef.__PVT__complexRwStage__DOT__pipeReg
         [0U][3U] >> 0x14U);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRwReg
            [0U]) | (IData)(complexRwStage__DOT____Vlvbound_h8bf3b354__0));
    vlSelfRef.__PVT__complexRwStage__DOT__unnamedblk4__DOT__i = 1U;
    vlSelfRef.__PVT__fpRwStage__DOT__stall = (1U & 
                                              ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                               >> 1U));
    vlSelfRef.__PVT__fpRwStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    fpRwStage__DOT____Vlvbound_hb611356e__0[0U] = (
                                                   (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                    [0U][2U] 
                                                    << 0x1aU) 
                                                   | (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                      [0U][1U] 
                                                      >> 6U));
    fpRwStage__DOT____Vlvbound_hb611356e__0[1U] = (
                                                   (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                    [0U][3U] 
                                                    << 0x1aU) 
                                                   | (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                      [0U][2U] 
                                                      >> 6U));
    fpRwStage__DOT____Vlvbound_hb611356e__0[2U] = (0x1fffffffU 
                                                   & ((vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                       [0U][4U] 
                                                       << 0x1aU) 
                                                      | (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                         [0U][3U] 
                                                         >> 6U)));
    vlSelfRef.__PVT__fpRwStage__DOT__iqData[0U][0U] 
        = fpRwStage__DOT____Vlvbound_hb611356e__0[0U];
    vlSelfRef.__PVT__fpRwStage__DOT__iqData[0U][1U] 
        = fpRwStage__DOT____Vlvbound_hb611356e__0[1U];
    vlSelfRef.__PVT__fpRwStage__DOT__iqData[0U][2U] 
        = fpRwStage__DOT____Vlvbound_hb611356e__0[2U];
    fpRwStage__DOT____Vlvbound_h0285a07a__0 = (1U & 
                                               (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                [0U][1U] 
                                                >> 5U));
    vlSelfRef.__PVT__fpRwStage__DOT__regValid[0U] = fpRwStage__DOT____Vlvbound_h0285a07a__0;
    fpRwStage__DOT____Vlvbound_h144dd5b6__0 = (1U & 
                                               (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                [0U][4U] 
                                                >> 3U));
    vlSelfRef.__PVT__fpRwStage__DOT__valid[0U] = fpRwStage__DOT____Vlvbound_h144dd5b6__0;
    __Vfunc_SelectiveFlushDetector__630__opPtr = (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__630__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__630__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__630__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__630__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__630__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__630__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 1U;
                goto __Vlabel2;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__630__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__630__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__630__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__630__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__630__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 1U;
                    goto __Vlabel2;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 0U;
                    goto __Vlabel2;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__630__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__630__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__630__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__630__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__630__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 1U;
                    goto __Vlabel2;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__630__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__630__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__630__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 1U;
                    goto __Vlabel2;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 0U;
                    goto __Vlabel2;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 0U;
                goto __Vlabel2;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout = 0U;
        }
        __Vlabel2: ;
    }
    fpRwStage__DOT____Vlvbound_h046b483c__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__630__Vfuncout;
    vlSelfRef.__PVT__fpRwStage__DOT__flush[0U] = fpRwStage__DOT____Vlvbound_h046b483c__0;
    fpRwStage__DOT____Vlvbound_h8546e2b5__0 = ((((~ (IData)(vlSelfRef.__PVT__fpRwStage__DOT__stall)) 
                                                 & (~ (IData)(vlSelfRef.__PVT__fpRwStage__DOT__clear))) 
                                                & vlSelfRef.__PVT__fpRwStage__DOT__valid
                                                [0U]) 
                                               & (~ 
                                                  vlSelfRef.__PVT__fpRwStage__DOT__flush
                                                  [0U]));
    vlSelfRef.__PVT__fpRwStage__DOT__update[0U] = fpRwStage__DOT____Vlvbound_h8546e2b5__0;
    fpRwStage__DOT____Vlvbound_h21fb6822__0 = (vlSelfRef.__PVT__fpRwStage__DOT__update
                                               [0U] 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][0U] 
                                                  >> 0x1cU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegWE[0U] 
        = fpRwStage__DOT____Vlvbound_h21fb6822__0;
    fpRwStage__DOT____Vlvbound_h6f02ee4c__0 = (0x7fU 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][0U] 
                                                  >> 0x15U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegNum[0U] 
        = fpRwStage__DOT____Vlvbound_h6f02ee4c__0;
    fpRwStage__DOT____Vlvbound_he6dc3282__0 = (0x1ffffffffULL 
                                               & (((QData)((IData)(
                                                                   vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                                   [0U][1U])) 
                                                   << 0x1bU) 
                                                  | ((QData)((IData)(
                                                                     vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                                     [0U][0U])) 
                                                     >> 5U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__fpDstRegData[0U] 
        = fpRwStage__DOT____Vlvbound_he6dc3282__0;
    fpRwStage__DOT____Vlvbound_h8cf597d3__0 = (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                               [0U][1U] 
                                               >> 0x1aU);
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][2U] 
        = ((3U & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][2U]) | (0xffU & ((IData)(fpRwStage__DOT____Vlvbound_h8cf597d3__0) 
                                  << 2U)));
    fpRwStage__DOT____Vlvbound_h851d8249__0 = (0xfU 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x16U));
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][1U]) | ((IData)(fpRwStage__DOT____Vlvbound_h851d8249__0) 
                         << 0x1eU));
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][2U]) | (0xffU & ((IData)(fpRwStage__DOT____Vlvbound_h851d8249__0) 
                                  >> 2U)));
    fpRwStage__DOT____Vlvbound_h851db172__0 = (0xfU 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x12U));
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][1U]) | ((IData)(fpRwStage__DOT____Vlvbound_h851db172__0) 
                         << 0x1aU));
    fpRwStage__DOT____Vlvbound_h7f5b6f62__0 = (0xfffffU 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__iqData
                                                  [0U][0U] 
                                                  >> 1U));
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][1U]) | (fpRwStage__DOT____Vlvbound_h7f5b6f62__0 
                         << 2U));
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][0U] 
        = (3U & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][1U] 
        = (0xfffffffcU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
           [0U][1U]);
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffdU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffeU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__fpRwStage__DOT__alWriteData[0U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
            [0U][1U]) | (((vlSelfRef.__PVT__fpRwStage__DOT__update
                           [0U] & vlSelfRef.__PVT__fpRwStage__DOT__regValid
                           [0U]) ? 1U : 0U) << 0x16U));
    fpRwStage__DOT____Vlvbound_he995c148__0 = vlSelfRef.__PVT__fpRwStage__DOT__update
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWrite[0U] 
        = fpRwStage__DOT____Vlvbound_he995c148__0;
    fpRwStage__DOT____Vlvbound_h8af695fc__0[0U] = vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
        [0U][0U];
    fpRwStage__DOT____Vlvbound_h8af695fc__0[1U] = vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
        [0U][1U];
    fpRwStage__DOT____Vlvbound_h8af695fc__0[2U] = vlSelfRef.__PVT__fpRwStage__DOT__alWriteData
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData[0U][0U] 
        = fpRwStage__DOT____Vlvbound_h8af695fc__0[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData[0U][1U] 
        = fpRwStage__DOT____Vlvbound_h8af695fc__0[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpWriteData[0U][2U] 
        = fpRwStage__DOT____Vlvbound_h8af695fc__0[2U];
    fpRwStage__DOT____Vlvbound_h1d55e97c__0 = (0x1fU 
                                               & vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                               [0U][0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__fpFFlagsData[0U] 
        = fpRwStage__DOT____Vlvbound_h1d55e97c__0;
    vlSelfRef.__PVT__fpRwStage__DOT__unnamedblk3__DOT__i = 1U;
    fpRwStage__DOT____Vlvbound_h48155c9e__0 = vlSelfRef.__PVT__fpRwStage__DOT__valid
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg
            [0U]) | ((IData)(fpRwStage__DOT____Vlvbound_h48155c9e__0) 
                     << 0xdU));
    fpRwStage__DOT____Vlvbound_h48154c0c__0 = vlSelfRef.__PVT__fpRwStage__DOT__flush
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg
            [0U]) | ((IData)(fpRwStage__DOT____Vlvbound_h48154c0c__0) 
                     << 0xcU));
    fpRwStage__DOT____Vlvbound_h5e9ebd83__0 = (0xfffU 
                                               & (vlSelfRef.__PVT__fpRwStage__DOT__pipeReg
                                                  [0U][4U] 
                                                  >> 4U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__fpRwReg
            [0U]) | (IData)(fpRwStage__DOT____Vlvbound_h5e9ebd83__0));
    vlSelfRef.__PVT__fpRwStage__DOT__unnamedblk4__DOT__i = 1U;
    vlSelfRef.__PVT__intRwStage__DOT__stall = (1U & 
                                               ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                >> 1U));
    vlSelfRef.__PVT__intRwStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[0U][0U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][3U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][2U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[0U][1U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][4U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][3U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[0U][2U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][5U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][4U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[0U][3U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][6U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][5U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[0U][4U] 
        = (0x7ffU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                      [0U][7U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                         [0U][6U] >> 0x1bU)));
    vlSelfRef.__PVT__intRwStage__DOT__regValid[0U] 
        = (1U & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                 [0U][2U] >> 0x1aU));
    vlSelfRef.__PVT__intRwStage__DOT__valid[0U] = (1U 
                                                   & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                      [0U][7U] 
                                                      >> 6U));
    __Vfunc_SelectiveFlushDetector__548__opPtr = (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__548__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__548__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__548__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__548__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__548__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__548__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                goto __Vlabel3;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__548__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel3;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                    goto __Vlabel3;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__548__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel3;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel3;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                    goto __Vlabel3;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                goto __Vlabel3;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
        }
        __Vlabel3: ;
    }
    vlSelfRef.__PVT__intRwStage__DOT__flush[0U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout;
    vlSelfRef.__PVT__intRwStage__DOT__update[0U] = 
        ((((~ (IData)(vlSelfRef.__PVT__intRwStage__DOT__stall)) 
           & (~ (IData)(vlSelfRef.__PVT__intRwStage__DOT__clear))) 
          & vlSelfRef.__PVT__intRwStage__DOT__valid
          [0U]) & (~ vlSelfRef.__PVT__intRwStage__DOT__flush
                   [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegWE[0U] 
        = (vlSelfRef.__PVT__intRwStage__DOT__update
           [0U] & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                   [0U][0U] >> 0x1cU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum[0U] 
        = (0x7fU & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                    [0U][0U] >> 0x15U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegData[0U] 
        = (0x1ffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                              [0U][2U])) 
                              << 6U) | ((QData)((IData)(
                                                        vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                        [0U][1U])) 
                                        >> 0x1aU)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][2U] 
        = ((3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][2U]) | (0xfcU & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                  [0U][1U] >> 0x18U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][1U]) | (0xc0000000U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [0U][1U] << 8U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][2U]) | (3U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                               [0U][1U] >> 0x18U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][1U]) | (0x3c000000U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [0U][1U] << 8U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][1U]) | (0x3ffffcU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                       [0U][1U] << 0x11U) 
                                      | (0x1fffcU & 
                                         (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                          [0U][0U] 
                                          >> 0xfU)))));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][0U] 
        = (3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
        = (0xfffffffcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [0U][1U]);
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][0U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][0U]) | (((2U == (7U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [0U][2U] >> 3U))) 
                          | (3U == (7U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                          [0U][2U] 
                                          >> 3U)))) 
                         << 1U));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffeU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__intRwStage__DOT__brResult[0U] 
        = (0x1ffffffffffffffULL & (((QData)((IData)(
                                                    vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                    [0U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                                [0U][0U]))));
    vlSelfRef.__PVT__intRwStage__DOT__brResult[0U] 
        = ((0x1ffffffffffefffULL & vlSelfRef.__PVT__intRwStage__DOT__brResult
            [0U]) | ((QData)((IData)((((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                        [0U][0U] >> 0xcU) 
                                       & vlSelfRef.__PVT__intRwStage__DOT__update
                                       [0U]) & vlSelfRef.__PVT__intRwStage__DOT__regValid
                                      [0U]))) << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult[0U] 
        = vlSelfRef.__PVT__intRwStage__DOT__brResult
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult[1U] 
        = vlSelfRef.__PVT__intRwStage__DOT__brResult
        [1U];
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [0U][1U]) | ((vlSelfRef.__PVT__intRwStage__DOT__update
                          [0U] ? (vlSelfRef.__PVT__intRwStage__DOT__regValid
                                  [0U] ? ((0x2000000U 
                                           & vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                           [0U][1U])
                                           ? 3U : 1U)
                                   : 0U) : 0U) << 0x16U));
    if (((3U == (0xfU & (vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                         [0U][1U] >> 0x16U))) | (1U 
                                                 == 
                                                 (0xfU 
                                                  & (vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                                                     [0U][1U] 
                                                     >> 0x16U))))) {
        if (((0U != (3U & (IData)((vlSelfRef.__PVT__intRwStage__DOT__brResult
                                   [0U] >> 0x11U)))) 
             & (IData)((vlSelfRef.__PVT__intRwStage__DOT__brResult
                        [0U] >> 0xcU)))) {
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
                = (0x3800000U | (0xfc3fffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                                 [0U][1U]));
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][0U] 
                = ((3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                    [0U][0U]) | (0x3ffffcU & ((IData)(
                                                      (vlSelfRef.__PVT__intRwStage__DOT__brResult
                                                       [0U] 
                                                       >> 0x11U)) 
                                              << 2U)));
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[0U][1U] 
                = (0xfffffffcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                   [0U][1U]);
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWrite[0U] 
        = vlSelfRef.__PVT__intRwStage__DOT__update[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[0U][0U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[0U][1U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[0U][2U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordEntry[0U] 
        = (vlSelfRef.__PVT__intRwStage__DOT__update
           [0U] & (~ vlSelfRef.__PVT__intRwStage__DOT__regValid
                   [0U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[0U][0U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][3U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][2U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[0U][1U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][4U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][3U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[0U][2U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][5U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][4U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[0U][3U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [0U][6U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][5U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[0U][4U] 
        = (0x7ffU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                      [0U][7U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                         [0U][6U] >> 0x1bU)));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[1U][0U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][3U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][2U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[1U][1U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][4U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][3U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[1U][2U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][5U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][4U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[1U][3U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][6U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][5U] >> 0x1bU));
    vlSelfRef.__PVT__intRwStage__DOT__iqData[1U][4U] 
        = (0x7ffU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                      [1U][7U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                         [1U][6U] >> 0x1bU)));
    vlSelfRef.__PVT__intRwStage__DOT__regValid[1U] 
        = (1U & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                 [1U][2U] >> 0x1aU));
    vlSelfRef.__PVT__intRwStage__DOT__valid[1U] = (1U 
                                                   & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                      [1U][7U] 
                                                      >> 6U));
    __Vfunc_SelectiveFlushDetector__548__opPtr = (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                                  [1U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__548__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__548__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__548__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__548__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__548__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__548__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                goto __Vlabel4;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__548__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel4;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                    goto __Vlabel4;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__548__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel4;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__548__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__548__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__548__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 1U;
                    goto __Vlabel4;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                    goto __Vlabel4;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
                goto __Vlabel4;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout = 0U;
        }
        __Vlabel4: ;
    }
    vlSelfRef.__PVT__intRwStage__DOT__flush[1U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__548__Vfuncout;
    vlSelfRef.__PVT__intRwStage__DOT__update[1U] = 
        ((((~ (IData)(vlSelfRef.__PVT__intRwStage__DOT__stall)) 
           & (~ (IData)(vlSelfRef.__PVT__intRwStage__DOT__clear))) 
          & vlSelfRef.__PVT__intRwStage__DOT__valid
          [1U]) & (~ vlSelfRef.__PVT__intRwStage__DOT__flush
                   [1U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegWE[1U] 
        = (vlSelfRef.__PVT__intRwStage__DOT__update
           [1U] & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                   [1U][0U] >> 0x1cU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegNum[1U] 
        = (0x7fU & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                    [1U][0U] >> 0x15U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intDstRegData[1U] 
        = (0x1ffffffffULL & (((QData)((IData)(vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                              [1U][2U])) 
                              << 6U) | ((QData)((IData)(
                                                        vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                        [1U][1U])) 
                                        >> 0x1aU)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][2U] 
        = ((3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][2U]) | (0xfcU & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                  [1U][1U] >> 0x18U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][1U]) | (0xc0000000U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [1U][1U] << 8U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][2U]) | (3U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                               [1U][1U] >> 0x18U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][1U]) | (0x3c000000U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [1U][1U] << 8U)));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][1U]) | (0x3ffffcU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                       [1U][1U] << 0x11U) 
                                      | (0x1fffcU & 
                                         (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                          [1U][0U] 
                                          >> 0xfU)))));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][0U] 
        = (3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [1U][0U]);
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
        = (0xfffffffcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [1U][1U]);
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][0U] 
        = ((0xfffffffdU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][0U]) | (((2U == (7U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                        [1U][2U] >> 3U))) 
                          | (3U == (7U & (vlSelfRef.__PVT__intRwStage__DOT__iqData
                                          [1U][2U] 
                                          >> 3U)))) 
                         << 1U));
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][0U] 
        = (0xfffffffeU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
           [1U][0U]);
    vlSelfRef.__PVT__intRwStage__DOT__brResult[1U] 
        = (0x1ffffffffffffffULL & (((QData)((IData)(
                                                    vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                    [1U][1U])) 
                                    << 0x20U) | (QData)((IData)(
                                                                vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                                                [1U][0U]))));
    vlSelfRef.__PVT__intRwStage__DOT__brResult[1U] 
        = ((0x1ffffffffffefffULL & vlSelfRef.__PVT__intRwStage__DOT__brResult
            [1U]) | ((QData)((IData)((((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                        [1U][0U] >> 0xcU) 
                                       & vlSelfRef.__PVT__intRwStage__DOT__update
                                       [1U]) & vlSelfRef.__PVT__intRwStage__DOT__regValid
                                      [1U]))) << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult[0U] 
        = vlSelfRef.__PVT__intRwStage__DOT__brResult
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__npStageIF.__PVT__brResult[1U] 
        = vlSelfRef.__PVT__intRwStage__DOT__brResult
        [1U];
    vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
            [1U][1U]) | ((vlSelfRef.__PVT__intRwStage__DOT__update
                          [1U] ? (vlSelfRef.__PVT__intRwStage__DOT__regValid
                                  [1U] ? ((0x2000000U 
                                           & vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                           [1U][1U])
                                           ? 3U : 1U)
                                   : 0U) : 0U) << 0x16U));
    if (((3U == (0xfU & (vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                         [1U][1U] >> 0x16U))) | (1U 
                                                 == 
                                                 (0xfU 
                                                  & (vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                                                     [1U][1U] 
                                                     >> 0x16U))))) {
        if (((0U != (3U & (IData)((vlSelfRef.__PVT__intRwStage__DOT__brResult
                                   [1U] >> 0x11U)))) 
             & (IData)((vlSelfRef.__PVT__intRwStage__DOT__brResult
                        [1U] >> 0xcU)))) {
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
                = (0x3800000U | (0xfc3fffffU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                                 [1U][1U]));
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][0U] 
                = ((3U & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                    [1U][0U]) | (0x3ffffcU & ((IData)(
                                                      (vlSelfRef.__PVT__intRwStage__DOT__brResult
                                                       [1U] 
                                                       >> 0x11U)) 
                                              << 2U)));
            vlSelfRef.__PVT__intRwStage__DOT__alWriteData[1U][1U] 
                = (0xfffffffcU & vlSelfRef.__PVT__intRwStage__DOT__alWriteData
                   [1U][1U]);
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWrite[1U] 
        = vlSelfRef.__PVT__intRwStage__DOT__update[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[1U][0U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[1U][1U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__intWriteData[1U][2U] 
        = vlSelfRef.__PVT__intRwStage__DOT__alWriteData
        [1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordEntry[1U] 
        = (vlSelfRef.__PVT__intRwStage__DOT__update
           [1U] & (~ vlSelfRef.__PVT__intRwStage__DOT__regValid
                   [1U]));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[1U][0U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][3U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][2U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[1U][1U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][4U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][3U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[1U][2U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][5U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][4U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[1U][3U] 
        = ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
            [1U][6U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][5U] >> 0x1bU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intRecordData[1U][4U] 
        = (0x7ffU & ((vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                      [1U][7U] << 5U) | (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                                         [1U][6U] >> 0x1bU)));
    vlSelfRef.__PVT__intRwStage__DOT__unnamedblk3__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [0U]) | (vlSelfRef.__PVT__intRwStage__DOT__valid
                     [0U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [0U]) | (vlSelfRef.__PVT__intRwStage__DOT__flush
                     [0U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [0U]) | (0xfffU & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [0U][7U] >> 7U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[1U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [1U]) | (vlSelfRef.__PVT__intRwStage__DOT__valid
                     [1U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[1U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [1U]) | (vlSelfRef.__PVT__intRwStage__DOT__flush
                     [1U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg[1U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRwReg
            [1U]) | (0xfffU & (vlSelfRef.__PVT__intRwStage__DOT__pipeReg
                               [1U][7U] >> 7U)));
    vlSelfRef.__PVT__intRwStage__DOT__unnamedblk4__DOT__i = 2U;
    vlSelfRef.__PVT__memRwStage__DOT__stall = (1U & 
                                               ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                >> 1U));
    vlSelfRef.__PVT__memRwStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect[0U] = 0U;
    if ((1U & ((((~ (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                     [0U][0U] >> 1U)) & (((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                           [0U][1U] 
                                           >> 3U) & 
                                          (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                           [0U][0U] 
                                           >> 2U)) 
                                         | vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                         [0U][0U])) 
                & vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                [0U][4U]) & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                             [0U][1U] >> 4U)))) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect[0U] = 1U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect[1U] = 0U;
    if ((1U & ((((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                  [0U][0U] >> 1U) & (((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                       [0U][1U] >> 3U) 
                                      & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                         [0U][0U] >> 2U)) 
                                     | vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                     [0U][0U])) & vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                [0U][4U]) & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                             [0U][1U] >> 4U)))) {
        vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect[1U] = 1U;
    }
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk3__DOT__unnamedblk4__DOT__i = 1U;
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk3__DOT__j = 2U;
    vlSelfRef.__PVT__memRwStage__DOT__valid[0U] = (1U 
                                                   & vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                   [0U][4U]);
    __Vfunc_SelectiveFlushDetector__618__opPtr = (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                  [0U][3U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__618__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__618__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__618__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__618__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__618__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__618__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                goto __Vlabel5;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__618__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel5;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                    goto __Vlabel5;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__618__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel5;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel5;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                    goto __Vlabel5;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                goto __Vlabel5;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
        }
        __Vlabel5: ;
    }
    vlSelfRef.__PVT__memRwStage__DOT__flush[0U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout;
    vlSelfRef.__PVT__memRwStage__DOT__update[0U] = 
        ((((~ (IData)(vlSelfRef.__PVT__memRwStage__DOT__stall)) 
           & (~ (IData)(vlSelfRef.__PVT__memRwStage__DOT__clear))) 
          & vlSelfRef.__PVT__memRwStage__DOT__valid
          [0U]) & (~ vlSelfRef.__PVT__memRwStage__DOT__flush
                   [0U]));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][2U] 
        = ((3U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][2U]) | (0xfcU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                  [0U][3U] >> 0x18U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][1U]) | (0xc0000000U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                        [0U][3U] << 8U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][2U]) | (3U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [0U][3U] >> 0x18U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][1U]) | (0x3c000000U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                        [0U][3U] << 8U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][0U] 
        = (0xfffffffdU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
           [0U][0U]);
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][0U]) | (1U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [0U][1U] >> 5U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][1U]) | ((vlSelfRef.__PVT__memRwStage__DOT__update
                          [0U] ? (0xfU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                          [0U][1U] 
                                          >> 6U)) : 0U) 
                         << 0x16U));
    vlSelfRef.__PVT__memRwStage__DOT__execState[0U] 
        = (0xfU & (vlSelfRef.__PVT__memRwStage__DOT__alWriteData
                   [0U][1U] >> 0x16U));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][1U]) | (0x3ffffcU & ((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                       [0U][3U] << 0x10U) 
                                      | (0xfffcU & 
                                         (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                          [0U][2U] 
                                          >> 0x10U)))));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][0U] 
        = ((3U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][0U]) | (0xfffffffcU & ((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                         [0U][2U] << 0x10U) 
                                        | (0xfffcU 
                                           & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                              [0U][1U] 
                                              >> 0x10U)))));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[0U][1U] 
        = ((0xfffffffcU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [0U][1U]) | (3U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [0U][2U] >> 0x10U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWrite[0U] 
        = vlSelfRef.__PVT__memRwStage__DOT__update[0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[0U][0U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[0U][1U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[0U][2U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [0U][2U];
    vlSelfRef.__PVT__memRwStage__DOT__valid[1U] = (1U 
                                                   & vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                   [1U][4U]);
    __Vfunc_SelectiveFlushDetector__618__opPtr = (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                  [1U][3U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__618__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__618__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__618__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__618__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__618__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__618__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                goto __Vlabel6;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__618__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel6;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                    goto __Vlabel6;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__618__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel6;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__618__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__618__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__618__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 1U;
                    goto __Vlabel6;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                    goto __Vlabel6;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
                goto __Vlabel6;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout = 0U;
        }
        __Vlabel6: ;
    }
    vlSelfRef.__PVT__memRwStage__DOT__flush[1U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__618__Vfuncout;
    vlSelfRef.__PVT__memRwStage__DOT__update[1U] = 
        ((((~ (IData)(vlSelfRef.__PVT__memRwStage__DOT__stall)) 
           & (~ (IData)(vlSelfRef.__PVT__memRwStage__DOT__clear))) 
          & vlSelfRef.__PVT__memRwStage__DOT__valid
          [1U]) & (~ vlSelfRef.__PVT__memRwStage__DOT__flush
                   [1U]));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][2U] 
        = ((3U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][2U]) | (0xfcU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                  [1U][3U] >> 0x18U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][1U] 
        = ((0x3fffffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][1U]) | (0xc0000000U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                        [1U][3U] << 8U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][2U] 
        = ((0xfcU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][2U]) | (3U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [1U][3U] >> 0x18U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][1U] 
        = ((0xc3ffffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][1U]) | (0x3c000000U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                        [1U][3U] << 8U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][0U] 
        = (0xfffffffdU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
           [1U][0U]);
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][0U] 
        = ((0xfffffffeU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][0U]) | (1U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [1U][1U] >> 5U)));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][1U] 
        = ((0xfc3fffffU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][1U]) | ((vlSelfRef.__PVT__memRwStage__DOT__update
                          [1U] ? (0xfU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                          [1U][1U] 
                                          >> 6U)) : 0U) 
                         << 0x16U));
    vlSelfRef.__PVT__memRwStage__DOT__execState[1U] 
        = (0xfU & (vlSelfRef.__PVT__memRwStage__DOT__alWriteData
                   [1U][1U] >> 0x16U));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][1U] 
        = ((0xffc00003U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][1U]) | (0x3ffffcU & ((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                       [1U][3U] << 0x10U) 
                                      | (0xfffcU & 
                                         (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                          [1U][2U] 
                                          >> 0x10U)))));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][0U] 
        = ((3U & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][0U]) | (0xfffffffcU & ((vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                         [1U][2U] << 0x10U) 
                                        | (0xfffcU 
                                           & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                              [1U][1U] 
                                              >> 0x10U)))));
    vlSelfRef.__PVT__memRwStage__DOT__alWriteData[1U][1U] 
        = ((0xfffffffcU & vlSelfRef.__PVT__memRwStage__DOT__alWriteData
            [1U][1U]) | (3U & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [1U][2U] >> 0x10U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWrite[1U] 
        = vlSelfRef.__PVT__memRwStage__DOT__update[1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[1U][0U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[1U][1U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__activeListIF.__PVT__memWriteData[1U][2U] 
        = vlSelfRef.__PVT__memRwStage__DOT__alWriteData
        [1U][2U];
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk5__DOT__i = 2U;
    memRwStage__DOT____Vlvbound_hb4b12956__0 = (vlSelfRef.__PVT__memRwStage__DOT__update
                                                [0U] 
                                                & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                   [0U][1U] 
                                                   >> 0x11U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegWE[0U] 
        = memRwStage__DOT____Vlvbound_hb4b12956__0;
    memRwStage__DOT____Vlvbound_hca7847bb__0 = (0x7fU 
                                                & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                   [0U][1U] 
                                                   >> 0xaU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegNum[0U] 
        = memRwStage__DOT____Vlvbound_hca7847bb__0;
    memRwStage__DOT____Vlvbound_h6f0537ce__0 = (0x1ffffffffULL 
                                                & (((QData)((IData)(
                                                                    vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                                    [0U][1U])) 
                                                    << 0x1dU) 
                                                   | ((QData)((IData)(
                                                                      vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                                                                      [0U][0U])) 
                                                      >> 3U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__memDstRegData[0U] 
        = memRwStage__DOT____Vlvbound_h6f0537ce__0;
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk6__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [0U]) | (vlSelfRef.__PVT__memRwStage__DOT__valid
                     [0U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [0U]) | (vlSelfRef.__PVT__memRwStage__DOT__flush
                     [0U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [0U]) | (0xfffU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [0U][4U] >> 1U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[1U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [1U]) | (vlSelfRef.__PVT__memRwStage__DOT__valid
                     [1U] << 0xdU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[1U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [1U]) | (vlSelfRef.__PVT__memRwStage__DOT__flush
                     [1U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg[1U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__memRwReg
            [1U]) | (0xfffU & (vlSelfRef.__PVT__memRwStage__DOT__pipeReg
                               [1U][4U] >> 1U)));
    vlSelfRef.__PVT__memRwStage__DOT__unnamedblk7__DOT__i = 2U;
    vlSelfRef.__PVT__scheduler__DOT__dispatchStore[0U] 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
           [0U] && ((2U == (3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                          [0U] >> 0x2fU)))) 
                    && (1U == (7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                             [0U] >> 0x2cU))))));
    vlSelfRef.__PVT__scheduler__DOT__dispatchStore[1U] 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
           [1U] && ((2U == (3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                          [1U] >> 0x2fU)))) 
                    && (1U == (7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                             [1U] >> 0x2cU))))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchStore[0U] 
        = vlSelfRef.__PVT__scheduler__DOT__dispatchStore
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchStore[1U] 
        = vlSelfRef.__PVT__scheduler__DOT__dispatchStore
        [1U];
    vlSelfRef.__PVT__scheduler__DOT__dispatchLoad[0U] 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
           [0U] && ((2U == (3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                          [0U] >> 0x2fU)))) 
                    && (1U != (7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                             [0U] >> 0x2cU))))));
    vlSelfRef.__PVT__scheduler__DOT__dispatchLoad[1U] 
        = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
           [1U] && ((2U == (3U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                          [1U] >> 0x2fU)))) 
                    && (1U != (7U & (IData)((vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__writeSchedulerData
                                             [1U] >> 0x2cU))))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchLoad[0U] 
        = vlSelfRef.__PVT__scheduler__DOT__dispatchLoad
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__dispatchLoad[1U] 
        = vlSelfRef.__PVT__scheduler__DOT__dispatchLoad
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__write[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__write
        [1U];
    vlSelfRef.__PVT__dCache__DOT__lsuMakeMSHRCanBeInvalidDirect[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect
        [0U];
    vlSelfRef.__PVT__dCache__DOT__lsuMakeMSHRCanBeInvalidDirect[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__makeMSHRCanBeInvalidDirect
        [1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCanBeInvalidDirect[0U] 
        = vlSelfRef.__PVT__dCache__DOT__lsuMakeMSHRCanBeInvalidDirect
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrCanBeInvalidDirect[1U] 
        = vlSelfRef.__PVT__dCache__DOT__lsuMakeMSHRCanBeInvalidDirect
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__1(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__cacheFlushManager__DOT__dcFlushReq = 0U;
    if ((1U & (~ ((IData)(vlSelfRef.__PVT__cacheFlushManager__DOT__regPhase) 
                  >> 1U)))) {
        if ((1U & (IData)(vlSelfRef.__PVT__cacheFlushManager__DOT__regPhase))) {
            if (((IData)(vlSelfRef.__PVT__iCache__DOT__regFlushReqAck) 
                 & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__dcFlushReqAck))) {
                vlSelfRef.__PVT__cacheFlushManager__DOT__dcFlushReq = 1U;
            }
        }
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__cacheSystemIF.__PVT__dcFlushReq 
        = vlSelfRef.__PVT__cacheFlushManager__DOT__dcFlushReq;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__2(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__2\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    VL_ASSIGNBIT_II(4U, vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg
                    [0U], (1U & ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)))
                                  ? ((vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                                      [0U] >> 4U) & 
                                     (~ ([&]() {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr 
                                    = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
                                       [0U][1U] >> 0x1aU);
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__flushAllInsns 
                                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 4U));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 0xaU));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange 
                                    = (1U == (3U & 
                                              (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)));
                                {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) {
                                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__530__flushAllInsns) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                            goto __Vlabel7;
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr) 
                                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel7;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                                goto __Vlabel7;
                                            }
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel7;
                                            } else if (
                                                       (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel7;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                                goto __Vlabel7;
                                            }
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                            goto __Vlabel7;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                    }
                                    __Vlabel7: ;
                                }
                            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout))))
                                  : (vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                                     [0U] >> 4U))));
    vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg[0U] 
        = ((0x10U & vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg
            [0U]) | (0xfU & vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                     [0U]));
    VL_ASSIGNBIT_II(4U, vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg
                    [1U], (1U & ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)))
                                  ? ((vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                                      [1U] >> 4U) & 
                                     (~ ([&]() {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr 
                                    = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__intIssuedData
                                       [1U][1U] >> 0x1aU);
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__flushAllInsns 
                                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 4U));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 0xaU));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange 
                                    = (1U == (3U & 
                                              (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)));
                                {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) {
                                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__530__flushAllInsns) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                            goto __Vlabel8;
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr) 
                                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel8;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                                goto __Vlabel8;
                                            }
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel8;
                                            } else if (
                                                       (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__headPtr)) 
                                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__opPtr) 
                                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 1U;
                                                goto __Vlabel8;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                                goto __Vlabel8;
                                            }
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                            goto __Vlabel8;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout = 0U;
                                    }
                                    __Vlabel8: ;
                                }
                            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__530__Vfuncout))))
                                  : (vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                                     [1U] >> 4U))));
    vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg[1U] 
        = ((0x10U & vlSelfRef.__PVT__intIsStage__DOT__nextPipeReg
            [1U]) | (0xfU & vlSelfRef.__PVT__intIsStage__DOT__pipeReg
                     [1U]));
    vlSelfRef.__PVT__intIsStage__DOT__unnamedblk2__DOT__i = 2U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__3(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__3\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    VL_ASSIGNBIT_II(4U, vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg
                    [0U], (1U & ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)))
                                  ? ((vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                                      [0U] >> 4U) & 
                                     (~ ([&]() {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr 
                                    = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
                                       [0U][1U] >> 0x1aU);
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__flushAllInsns 
                                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 4U));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 0xaU));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange 
                                    = (1U == (3U & 
                                              (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)));
                                {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) {
                                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__557__flushAllInsns) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                            goto __Vlabel9;
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr) 
                                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel9;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                                goto __Vlabel9;
                                            }
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel9;
                                            } else if (
                                                       (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel9;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                                goto __Vlabel9;
                                            }
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                            goto __Vlabel9;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                    }
                                    __Vlabel9: ;
                                }
                            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout))))
                                  : (vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                                     [0U] >> 4U))));
    vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg[0U] 
        = ((0x10U & vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg
            [0U]) | (0xfU & vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                     [0U]));
    VL_ASSIGNBIT_II(4U, vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg
                    [1U], (1U & ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)))
                                  ? ((vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                                      [1U] >> 4U) & 
                                     (~ ([&]() {
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr 
                                    = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__memIssuedData
                                       [1U][1U] >> 0x1aU);
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__flushAllInsns 
                                    = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 4U));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr 
                                    = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                                >> 0xaU));
                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange 
                                    = (1U == (3U & 
                                              (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                               >> 0x15U)));
                                {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) {
                                        if (vlSelfRef.__Vfunc_SelectiveFlushDetector__557__flushAllInsns) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                            goto __Vlabel10;
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr) 
                                                       >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                    < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel10;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                                goto __Vlabel10;
                                            }
                                        } else if (
                                                   ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__detectRange) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)))) {
                                            if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                  >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                 & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                    > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel10;
                                            } else if (
                                                       (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                         < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__headPtr)) 
                                                        & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__opPtr) 
                                                           < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__tailPtr)))) {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 1U;
                                                goto __Vlabel10;
                                            } else {
                                                vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                                goto __Vlabel10;
                                            }
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                            goto __Vlabel10;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout = 0U;
                                    }
                                    __Vlabel10: ;
                                }
                            }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__557__Vfuncout))))
                                  : (vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                                     [1U] >> 4U))));
    vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg[1U] 
        = ((0x10U & vlSelfRef.__PVT__memIsStage__DOT__nextPipeReg
            [1U]) | (0xfU & vlSelfRef.__PVT__memIsStage__DOT__pipeReg
                     [1U]));
    vlSelfRef.__PVT__memIsStage__DOT__unnamedblk2__DOT__i = 2U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__4(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__4\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*3:0*/ complexIsStage__DOT____Vlvbound_h83df4b47__0;
    complexIsStage__DOT____Vlvbound_h83df4b47__0 = 0;
    // Body
    if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                      >> 0x15U)))) {
        vlSelfRef.complexIsStage__DOT____Vlvbound_hbbf83b6a__0 
            = (1U & ((vlSelfRef.__PVT__complexIsStage__DOT__pipeReg
                      [0U] >> 4U) & (~ ([&]() {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr 
                                = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__complexIssuedData
                                   [0U][1U] >> 0x1aU);
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__flushAllInsns 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr 
                                = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                            >> 4U));
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr 
                                = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                            >> 0xaU));
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__detectRange 
                                = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                >> 0x15U)));
                            {
                                if (vlSelfRef.__Vfunc_SelectiveFlushDetector__549__detectRange) {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__549__flushAllInsns) {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 1U;
                                        goto __Vlabel11;
                                    } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__detectRange) 
                                                & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr) 
                                                   >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr)))) {
                                        if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                              >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr)) 
                                             & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                                < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 1U;
                                            goto __Vlabel11;
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 0U;
                                            goto __Vlabel11;
                                        }
                                    } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__detectRange) 
                                                & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr) 
                                                   < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr)))) {
                                        if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                              >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr)) 
                                             & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                                > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 1U;
                                            goto __Vlabel11;
                                        } else if (
                                                   (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                                     < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__headPtr)) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__opPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 1U;
                                            goto __Vlabel11;
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 0U;
                                            goto __Vlabel11;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 0U;
                                        goto __Vlabel11;
                                    }
                                } else {
                                    vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout = 0U;
                                }
                                __Vlabel11: ;
                            }
                        }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__549__Vfuncout)))));
        vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg[0U] 
            = ((0xfU & vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg
                [0U]) | ((IData)(vlSelfRef.complexIsStage__DOT____Vlvbound_hbbf83b6a__0) 
                         << 4U));
    } else {
        vlSelfRef.complexIsStage__DOT____Vlvbound_hbbf83b6a__1 
            = (1U & (vlSelfRef.__PVT__complexIsStage__DOT__pipeReg
                     [0U] >> 4U));
        vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg[0U] 
            = ((0xfU & vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg
                [0U]) | ((IData)(vlSelfRef.complexIsStage__DOT____Vlvbound_hbbf83b6a__1) 
                         << 4U));
    }
    complexIsStage__DOT____Vlvbound_h83df4b47__0 = 
        (0xfU & vlSelfRef.__PVT__complexIsStage__DOT__pipeReg
         [0U]);
    vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg[0U] 
        = ((0x10U & vlSelfRef.__PVT__complexIsStage__DOT__nextPipeReg
            [0U]) | (IData)(complexIsStage__DOT____Vlvbound_h83df4b47__0));
    vlSelfRef.__PVT__complexIsStage__DOT__unnamedblk2__DOT__i = 1U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__5(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__5\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    CData/*3:0*/ fpIsStage__DOT____Vlvbound_h83df4b47__0;
    fpIsStage__DOT____Vlvbound_h83df4b47__0 = 0;
    // Body
    if ((1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                      >> 0x15U)))) {
        vlSelfRef.fpIsStage__DOT____Vlvbound_hbbf83b6a__0 
            = (1U & ((vlSelfRef.__PVT__fpIsStage__DOT__pipeReg
                      [0U] >> 4U) & (~ ([&]() {
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr 
                                = (vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__fpIssuedData
                                   [0U][1U] >> 0x1aU);
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__flushAllInsns 
                                = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr 
                                = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                            >> 4U));
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr 
                                = (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                                            >> 0xaU));
                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__detectRange 
                                = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                                                >> 0x15U)));
                            {
                                if (vlSelfRef.__Vfunc_SelectiveFlushDetector__619__detectRange) {
                                    if (vlSelfRef.__Vfunc_SelectiveFlushDetector__619__flushAllInsns) {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 1U;
                                        goto __Vlabel12;
                                    } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__detectRange) 
                                                & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr) 
                                                   >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr)))) {
                                        if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                              >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr)) 
                                             & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                                < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 1U;
                                            goto __Vlabel12;
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 0U;
                                            goto __Vlabel12;
                                        }
                                    } else if (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__detectRange) 
                                                & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr) 
                                                   < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr)))) {
                                        if ((((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                              >= (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr)) 
                                             & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                                > (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 1U;
                                            goto __Vlabel12;
                                        } else if (
                                                   (((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                                     < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__headPtr)) 
                                                    & ((IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__opPtr) 
                                                       < (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__tailPtr)))) {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 1U;
                                            goto __Vlabel12;
                                        } else {
                                            vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 0U;
                                            goto __Vlabel12;
                                        }
                                    } else {
                                        vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 0U;
                                        goto __Vlabel12;
                                    }
                                } else {
                                    vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout = 0U;
                                }
                                __Vlabel12: ;
                            }
                        }(), (IData)(vlSelfRef.__Vfunc_SelectiveFlushDetector__619__Vfuncout)))));
        vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg[0U] 
            = ((0xfU & vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg
                [0U]) | ((IData)(vlSelfRef.fpIsStage__DOT____Vlvbound_hbbf83b6a__0) 
                         << 4U));
    } else {
        vlSelfRef.fpIsStage__DOT____Vlvbound_hbbf83b6a__1 
            = (1U & (vlSelfRef.__PVT__fpIsStage__DOT__pipeReg
                     [0U] >> 4U));
        vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg[0U] 
            = ((0xfU & vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg
                [0U]) | ((IData)(vlSelfRef.fpIsStage__DOT____Vlvbound_hbbf83b6a__1) 
                         << 4U));
    }
    fpIsStage__DOT____Vlvbound_h83df4b47__0 = (0xfU 
                                               & vlSelfRef.__PVT__fpIsStage__DOT__pipeReg
                                               [0U]);
    vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg[0U] 
        = ((0x10U & vlSelfRef.__PVT__fpIsStage__DOT__nextPipeReg
            [0U]) | (IData)(fpIsStage__DOT____Vlvbound_h83df4b47__0));
    vlSelfRef.__PVT__fpIsStage__DOT__unnamedblk2__DOT__i = 1U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__6(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__6\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__selectLogic__DOT__intRequest = 
        ((0xfffcU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__intRequest)) 
         | ((0xfffffffeU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [1U] << 1U))) | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                                                [0U])));
    vlSelfRef.__PVT__selectLogic__DOT__intRequest = 
        ((0xfff3U & (IData)(vlSelfRef.__PVT__selectLogic__DOT__intRequest)) 
         | ((0xfffffff8U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [3U] << 3U))) | (0xfffffffcU 
                                                & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                                                      [2U] 
                                                      << 2U)))));
    vlSelfRef.__PVT__selectLogic__DOT__intRequest = 
        ((0xffcfU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__intRequest)) 
         | ((0xffffffe0U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [5U] << 5U))) | (0xfffffff0U 
                                                & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                                                      [4U] 
                                                      << 4U)))));
    vlSelfRef.__PVT__selectLogic__DOT__intRequest = 
        ((0xff3fU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__intRequest)) 
         | ((0xffffff80U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [7U] << 7U))) | (0xffffffc0U 
                                                & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                                                      [6U] 
                                                      << 6U)))));
    vlSelfRef.__PVT__selectLogic__DOT__intRequest = 
        ((0xfcffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__intRequest)) 
         | ((0xfffffe00U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [9U] << 9U))) | (0xffffff00U 
                                                & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                   & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                                                      [8U] 
                                                      << 8U)))));
    vlSelfRef.__PVT__selectLogic__DOT__intRequest = 
        ((0xf3ffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__intRequest)) 
         | ((0xfffff800U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [0xbU] << 0xbU))) | 
            (0xfffffc00U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [0xaU] << 0xaU)))));
    vlSelfRef.__PVT__selectLogic__DOT__intRequest = 
        ((0xcfffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__intRequest)) 
         | ((0xffffe000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [0xdU] << 0xdU))) | 
            (0xfffff000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [0xcU] << 0xcU)))));
    vlSelfRef.__PVT__selectLogic__DOT__intRequest = 
        ((0x3fffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__intRequest)) 
         | ((0xffff8000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [0xfU] << 0xfU))) | 
            (0xffffc000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                            & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__intIssueReq
                               [0xeU] << 0xeU)))));
    vlSelfRef.__PVT__selectLogic__DOT__loadRequest 
        = ((0xfffcU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__loadRequest)) 
           | ((0xfffffffeU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                 [1U] << 1U))) | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                                  [0U])));
    vlSelfRef.__PVT__selectLogic__DOT__loadRequest 
        = ((0xfff3U & (IData)(vlSelfRef.__PVT__selectLogic__DOT__loadRequest)) 
           | ((0xfffffff8U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                 [3U] << 3U))) | (0xfffffffcU 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                                        [2U] 
                                                        << 2U)))));
    vlSelfRef.__PVT__selectLogic__DOT__loadRequest 
        = ((0xffcfU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__loadRequest)) 
           | ((0xffffffe0U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                 [5U] << 5U))) | (0xfffffff0U 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                                        [4U] 
                                                        << 4U)))));
    vlSelfRef.__PVT__selectLogic__DOT__loadRequest 
        = ((0xff3fU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__loadRequest)) 
           | ((0xffffff80U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                 [7U] << 7U))) | (0xffffffc0U 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                                        [6U] 
                                                        << 6U)))));
    vlSelfRef.__PVT__selectLogic__DOT__loadRequest 
        = ((0xfcffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__loadRequest)) 
           | ((0xfffffe00U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                 [9U] << 9U))) | (0xffffff00U 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                                        [8U] 
                                                        << 8U)))));
    vlSelfRef.__PVT__selectLogic__DOT__loadRequest 
        = ((0xf3ffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__loadRequest)) 
           | ((0xfffff800U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                 [0xbU] << 0xbU))) 
              | (0xfffffc00U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                   [0xaU] << 0xaU)))));
    vlSelfRef.__PVT__selectLogic__DOT__loadRequest 
        = ((0xcfffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__loadRequest)) 
           | ((0xffffe000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                 [0xdU] << 0xdU))) 
              | (0xfffff000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                   [0xcU] << 0xcU)))));
    vlSelfRef.__PVT__selectLogic__DOT__loadRequest 
        = ((0x3fffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__loadRequest)) 
           | ((0xffff8000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                 [0xfU] << 0xfU))) 
              | (0xffffc000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__loadIssueReq
                                   [0xeU] << 0xeU)))));
    vlSelfRef.__PVT__selectLogic__DOT__storeRequest 
        = ((0xfffcU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__storeRequest)) 
           | ((0xfffffffeU & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                 [1U] << 1U))) | ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                  & vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                                  [0U])));
    vlSelfRef.__PVT__selectLogic__DOT__storeRequest 
        = ((0xfff3U & (IData)(vlSelfRef.__PVT__selectLogic__DOT__storeRequest)) 
           | ((0xfffffff8U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                 [3U] << 3U))) | (0xfffffffcU 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                                        [2U] 
                                                        << 2U)))));
    vlSelfRef.__PVT__selectLogic__DOT__storeRequest 
        = ((0xffcfU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__storeRequest)) 
           | ((0xffffffe0U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                 [5U] << 5U))) | (0xfffffff0U 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                                        [4U] 
                                                        << 4U)))));
    vlSelfRef.__PVT__selectLogic__DOT__storeRequest 
        = ((0xff3fU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__storeRequest)) 
           | ((0xffffff80U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                 [7U] << 7U))) | (0xffffffc0U 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                                        [6U] 
                                                        << 6U)))));
    vlSelfRef.__PVT__selectLogic__DOT__storeRequest 
        = ((0xfcffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__storeRequest)) 
           | ((0xfffffe00U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                 [9U] << 9U))) | (0xffffff00U 
                                                  & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                                     & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                                        [8U] 
                                                        << 8U)))));
    vlSelfRef.__PVT__selectLogic__DOT__storeRequest 
        = ((0xf3ffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__storeRequest)) 
           | ((0xfffff800U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                 [0xbU] << 0xbU))) 
              | (0xfffffc00U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                   [0xaU] << 0xaU)))));
    vlSelfRef.__PVT__selectLogic__DOT__storeRequest 
        = ((0xcfffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__storeRequest)) 
           | ((0xffffe000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                 [0xdU] << 0xdU))) 
              | (0xfffff000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                   [0xcU] << 0xcU)))));
    vlSelfRef.__PVT__selectLogic__DOT__storeRequest 
        = ((0x3fffU & (IData)(vlSelfRef.__PVT__selectLogic__DOT__storeRequest)) 
           | ((0xffff8000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                              & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                 [0xfU] << 0xfU))) 
              | (0xffffc000U & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__opReady) 
                                & (vlSymsp->TOP__SMT_RTL_Testbench__core__wakeupSelectIF.__PVT__storeIssueReq
                                   [0xeU] << 0xeU)))));
    vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__reqTmp 
        = vlSelfRef.__PVT__selectLogic__DOT__intRequest;
    vlSelfRef.__PVT__selectLogic__DOT__intGrant = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__intSelected[0U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__intSelectedPtr[0U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 0U;
    {
        while (VL_GTS_III(32, 0x10U, vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)) {
            if ((1U & ((IData)(vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__reqTmp) 
                       >> (0xfU & vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)))) {
                vlSelfRef.__PVT__selectLogic__DOT__intGrant 
                    = ((IData)(vlSelfRef.__PVT__selectLogic__DOT__intGrant) 
                       | (0xffffU & ((IData)(1U) << 
                                     (0xfU & vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))));
                vlSelfRef.__PVT__selectLogic__DOT__intSelected[0U] = 1U;
                vlSelfRef.__PVT__selectLogic__DOT__intSelectedPtr[0U] 
                    = (0xfU & vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
                vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__reqTmp 
                    = ((~ ((IData)(1U) << (0xfU & vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))) 
                       & (IData)(vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__reqTmp));
                goto __Vlabel13;
            }
            vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e 
                = ((IData)(2U) + vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
        }
        __Vlabel13: ;
    }
    vlSelfRef.__PVT__selectLogic__DOT__intSelected[1U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__intSelectedPtr[1U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 1U;
    {
        while (VL_GTS_III(32, 0x10U, vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)) {
            if ((1U & ((IData)(vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__reqTmp) 
                       >> (0xfU & vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)))) {
                vlSelfRef.__PVT__selectLogic__DOT__intGrant 
                    = ((IData)(vlSelfRef.__PVT__selectLogic__DOT__intGrant) 
                       | (0xffffU & ((IData)(1U) << 
                                     (0xfU & vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))));
                vlSelfRef.__PVT__selectLogic__DOT__intSelected[1U] = 1U;
                vlSelfRef.__PVT__selectLogic__DOT__intSelectedPtr[1U] 
                    = (0xfU & vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
                vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__reqTmp 
                    = ((~ ((IData)(1U) << (0xfU & vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))) 
                       & (IData)(vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__reqTmp));
                goto __Vlabel14;
            }
            vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e 
                = ((IData)(2U) + vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
        }
        __Vlabel14: ;
    }
    vlSelfRef.__PVT__selectLogic__DOT__intPicker__DOT__unnamedblk1__DOT__p = 2U;
    vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__reqTmp 
        = vlSelfRef.__PVT__selectLogic__DOT__loadRequest;
    vlSelfRef.__PVT__selectLogic__DOT__loadGrant = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__loadSelected[0U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__loadSelectedPtr[0U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 0U;
    {
        while (VL_GTS_III(32, 0x10U, vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)) {
            if ((1U & ((IData)(vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__reqTmp) 
                       >> (0xfU & vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)))) {
                vlSelfRef.__PVT__selectLogic__DOT__loadGrant 
                    = ((IData)(vlSelfRef.__PVT__selectLogic__DOT__loadGrant) 
                       | (0xffffU & ((IData)(1U) << 
                                     (0xfU & vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))));
                vlSelfRef.selectLogic__DOT__loadPicker__DOT____Vlvbound_h1f89c34b__1 = 1U;
                vlSelfRef.__PVT__selectLogic__DOT__loadSelected[0U] 
                    = vlSelfRef.selectLogic__DOT__loadPicker__DOT____Vlvbound_h1f89c34b__1;
                vlSelfRef.selectLogic__DOT__loadPicker__DOT____Vlvbound_h54e39f63__1 
                    = (0xfU & vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
                vlSelfRef.__PVT__selectLogic__DOT__loadSelectedPtr[0U] 
                    = vlSelfRef.selectLogic__DOT__loadPicker__DOT____Vlvbound_h54e39f63__1;
                vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__reqTmp 
                    = ((~ ((IData)(1U) << (0xfU & vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))) 
                       & (IData)(vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__reqTmp));
                goto __Vlabel15;
            }
            vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e 
                = ((IData)(1U) + vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
        }
        __Vlabel15: ;
    }
    vlSelfRef.__PVT__selectLogic__DOT__loadPicker__DOT__unnamedblk1__DOT__p = 1U;
    vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__reqTmp 
        = vlSelfRef.__PVT__selectLogic__DOT__storeRequest;
    vlSelfRef.__PVT__selectLogic__DOT__storeGrant = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__storeSelected[0U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__storeSelectedPtr[0U] = 0U;
    vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e = 0U;
    {
        while (VL_GTS_III(32, 0x10U, vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)) {
            if ((1U & ((IData)(vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__reqTmp) 
                       >> (0xfU & vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e)))) {
                vlSelfRef.__PVT__selectLogic__DOT__storeGrant 
                    = ((IData)(vlSelfRef.__PVT__selectLogic__DOT__storeGrant) 
                       | (0xffffU & ((IData)(1U) << 
                                     (0xfU & vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))));
                vlSelfRef.selectLogic__DOT__storePicker__DOT____Vlvbound_h1f89c34b__1 = 1U;
                vlSelfRef.__PVT__selectLogic__DOT__storeSelected[0U] 
                    = vlSelfRef.selectLogic__DOT__storePicker__DOT____Vlvbound_h1f89c34b__1;
                vlSelfRef.selectLogic__DOT__storePicker__DOT____Vlvbound_h54e39f63__1 
                    = (0xfU & vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
                vlSelfRef.__PVT__selectLogic__DOT__storeSelectedPtr[0U] 
                    = vlSelfRef.selectLogic__DOT__storePicker__DOT____Vlvbound_h54e39f63__1;
                vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__reqTmp 
                    = ((~ ((IData)(1U) << (0xfU & vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e))) 
                       & (IData)(vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__reqTmp));
                goto __Vlabel16;
            }
            vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e 
                = ((IData)(1U) + vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__unnamedblk2__DOT__e);
        }
        __Vlabel16: ;
    }
    vlSelfRef.__PVT__selectLogic__DOT__storePicker__DOT__unnamedblk1__DOT__p = 1U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__7(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__7\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__ioUnit__DOT__phyRawReadAddr = 
        (0xfffffU & vlSymsp->TOP__SMT_RTL_Testbench__core__ioUnitIF.__PVT__ioReadAddrIn);
    vlSymsp->TOP__SMT_RTL_Testbench__core__ioUnitIF.__PVT__ioReadDataOut 
        = ((0U == vlSelfRef.__PVT__ioUnit__DOT__phyRawReadAddr)
            ? vlSelfRef.__PVT__ioUnit__DOT__tmReg[2U]
            : ((4U == vlSelfRef.__PVT__ioUnit__DOT__phyRawReadAddr)
                ? vlSelfRef.__PVT__ioUnit__DOT__tmReg[3U]
                : ((8U == vlSelfRef.__PVT__ioUnit__DOT__phyRawReadAddr)
                    ? vlSelfRef.__PVT__ioUnit__DOT__tmReg[0U]
                    : vlSelfRef.__PVT__ioUnit__DOT__tmReg[1U])));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__8(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__8\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__grant[0U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__req[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq
        [0U];
    vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__grant[1U] = 0U;
    vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__req[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemReq
        [1U];
    vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__unnamedblk1__DOT__r = 2U;
    vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__memInSel = 0U;
    vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__memValid = 0U;
    vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__unnamedblk2__DOT__r = 0U;
    {
        while (VL_GTS_III(32, 2U, vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__unnamedblk2__DOT__r)) {
            if (vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__req
                [(1U & vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__unnamedblk2__DOT__r)]) {
                vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__grant[(1U 
                                                                      & vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__unnamedblk2__DOT__r)] = 1U;
                vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__memInSel 
                    = (1U & vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__unnamedblk2__DOT__r);
                vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__memValid = 1U;
                goto __Vlabel17;
            }
            vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__unnamedblk2__DOT__r 
                = ((IData)(1U) + vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__unnamedblk2__DOT__r);
        }
        __Vlabel17: ;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memInSel 
        = vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__memInSel;
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__memValid 
        = vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__memValid;
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemGrt[0U] 
        = vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__grant
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__dCache__DOT__port.__PVT__mshrMemGrt[1U] 
        = vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__grant
        [1U];
    vlSelfRef.__PVT__dCache__DOT__memArbiter__DOT__unnamedblk3__DOT__r = 2U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__9(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__9\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtrFromPipeReg[0U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocatedPtr
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__watWriteIssueQueuePtrFromPipeReg[1U] 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__schedulerIF.__PVT__allocatedPtr
        [1U];
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__10(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__10\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__PVT__rnStage__DOT__valid = ((2U & vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                             [1U][4U]) 
                                            | (1U & 
                                               (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                                                [0U][4U] 
                                                >> 1U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__rnStageSendBubbleLower 
        = (((0U != (IData)(vlSelfRef.__PVT__rnStage__DOT__valid)) 
            & ((((~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__renameLogicIF.__PVT__allocatable)) 
                 | (~ ((~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue.__PVT__freeListReset)) 
                       & (2U <= (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__issueQueue__issueQueueFreeList.__PVT__queuePointer__DOT__regCount))))) 
                | (0x3eU < (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount))) 
               | (~ (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__loadStoreUnitIF.__PVT__allocatable)))) 
           | (IData)(vlSelfRef.__PVT__rnStage__DOT__serialize));
    vlSelfRef.__PVT__rnStage__DOT__serialize = 0U;
    if ((0U == (IData)(vlSelfRef.__PVT__rnStage__DOT__serializer__DOT__regPhase))) {
        if ((1U & (vlSelfRef.__PVT__rnStage__DOT__opInfo[0U] 
                   & (IData)(vlSelfRef.__PVT__rnStage__DOT__valid)))) {
            if ((0x20000U & vlSelfRef.__PVT__rnStage__DOT__opInfo[1U])) {
                if (((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount)) 
                     | (0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount)))) {
                    vlSelfRef.__PVT__rnStage__DOT__serialize = 1U;
                }
            } else if ((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount))) {
                vlSelfRef.__PVT__rnStage__DOT__serialize = 1U;
            }
        }
    } else if (((0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__activeList.__PVT__activeListPointer__DOT__regCount)) 
                | (0U != (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__storeQueue.__PVT__storeQueuePointer__DOT__regCount)))) {
        vlSelfRef.__PVT__rnStage__DOT__serialize = 1U;
    }
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnReg[0U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnReg
            [0U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__rnStage__DOT__valid) 
                                << 0xcU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnReg[0U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnReg
            [0U]) | (0xfffU & (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                               [0U][4U] >> 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnReg[1U] 
        = ((0xfffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnReg
            [1U]) | (0x1000U & ((IData)(vlSelfRef.__PVT__rnStage__DOT__valid) 
                                << 0xbU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnReg[1U] 
        = ((0x1000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__rnReg
            [1U]) | (0xfffU & (vlSelfRef.__PVT__rnStage__DOT__pipeReg
                               [1U][4U] >> 2U)));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__11(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_sequent__TOP__SMT_RTL_Testbench__core__11\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__unableToStartRecovery 
        = ((((IData)(((0U != (0x600000U & vlSelfRef.__PVT__recoveryManager__DOT__regState[3U])) 
                      | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__renameLogicRecoveryRMT))) 
             | (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__issueQueueReturnIndex)) 
            | (0U != (IData)(vlSelfRef.__PVT__replayQueue__DOT__canBeFlushedEntryCount))) 
           | ((((IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountInt) 
                | (0U != (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountComplex))) 
               | (0U != (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountFP))) 
              | (0U != (IData)(vlSelfRef.__PVT__wakeupPipelineRegister__DOT__canBeFlushedRegCountMem))));
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__0(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__0\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    VlWide<3>/*81:0*/ complexRrStage__DOT____Vlvbound_h51bf7b14__0;
    VL_ZERO_W(82, complexRrStage__DOT____Vlvbound_h51bf7b14__0);
    CData/*2:0*/ complexRrStage__DOT____Vlvbound_h4f6daee4__0;
    complexRrStage__DOT____Vlvbound_h4f6daee4__0 = 0;
    IData/*20:0*/ complexRrStage__DOT____Vlvbound_h79f92e5a__0;
    complexRrStage__DOT____Vlvbound_h79f92e5a__0 = 0;
    CData/*7:0*/ complexRrStage__DOT____Vlvbound_h9435fd27__0;
    complexRrStage__DOT____Vlvbound_h9435fd27__0 = 0;
    CData/*6:0*/ complexRrStage__DOT____Vlvbound_h02c11781__0;
    complexRrStage__DOT____Vlvbound_h02c11781__0 = 0;
    CData/*6:0*/ complexRrStage__DOT____Vlvbound_ha2604c03__0;
    complexRrStage__DOT____Vlvbound_ha2604c03__0 = 0;
    CData/*6:0*/ complexRrStage__DOT____Vlvbound_h04fb3afd__0;
    complexRrStage__DOT____Vlvbound_h04fb3afd__0 = 0;
    CData/*6:0*/ complexRrStage__DOT____Vlvbound_h738e9198__0;
    complexRrStage__DOT____Vlvbound_h738e9198__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_h09578847__0;
    complexRrStage__DOT____Vlvbound_h09578847__0 = 0;
    CData/*6:0*/ complexRrStage__DOT____Vlvbound_h095fb107__0;
    complexRrStage__DOT____Vlvbound_h095fb107__0 = 0;
    SData/*11:0*/ complexRrStage__DOT____Vlvbound_h022db8e7__0;
    complexRrStage__DOT____Vlvbound_h022db8e7__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_h046b483c__0;
    complexRrStage__DOT____Vlvbound_h046b483c__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_ha5bb9e4e__0;
    complexRrStage__DOT____Vlvbound_ha5bb9e4e__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_ha630ed7f__0;
    complexRrStage__DOT____Vlvbound_ha630ed7f__0 = 0;
    QData/*32:0*/ complexRrStage__DOT____Vlvbound_h0a98cdc0__0;
    complexRrStage__DOT____Vlvbound_h0a98cdc0__0 = 0;
    QData/*32:0*/ complexRrStage__DOT____Vlvbound_h0a827eb3__0;
    complexRrStage__DOT____Vlvbound_h0a827eb3__0 = 0;
    VlWide<3>/*81:0*/ complexRrStage__DOT____Vlvbound_hb64b5082__0;
    VL_ZERO_W(82, complexRrStage__DOT____Vlvbound_hb64b5082__0);
    IData/*20:0*/ complexRrStage__DOT____Vlvbound_h094f5bee__0;
    complexRrStage__DOT____Vlvbound_h094f5bee__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_h31a1399f__0;
    complexRrStage__DOT____Vlvbound_h31a1399f__0 = 0;
    CData/*0:0*/ complexRrStage__DOT____Vlvbound_h31a147f0__0;
    complexRrStage__DOT____Vlvbound_h31a147f0__0 = 0;
    SData/*11:0*/ complexRrStage__DOT____Vlvbound_hbb48d9af__0;
    complexRrStage__DOT____Vlvbound_hbb48d9af__0 = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__551__detectRange;
    __Vfunc_SelectiveFlushDetector__551__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__551__headPtr;
    __Vfunc_SelectiveFlushDetector__551__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__551__tailPtr;
    __Vfunc_SelectiveFlushDetector__551__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__551__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__551__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__551__opPtr;
    __Vfunc_SelectiveFlushDetector__551__opPtr = 0;
    // Body
    vlSelfRef.__PVT__complexRrStage__DOT__stall = (1U 
                                                   & ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                      >> 1U));
    vlSelfRef.__PVT__complexRrStage__DOT__clear = (1U 
                                                   & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    complexRrStage__DOT____Vlvbound_h51bf7b14__0[0U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
        [0U][0U];
    complexRrStage__DOT____Vlvbound_h51bf7b14__0[1U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
        [0U][1U];
    complexRrStage__DOT____Vlvbound_h51bf7b14__0[2U] 
        = (0x3ffffU & vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
           [0U][2U]);
    vlSelfRef.__PVT__complexRrStage__DOT__iqData[0U][0U] 
        = complexRrStage__DOT____Vlvbound_h51bf7b14__0[0U];
    vlSelfRef.__PVT__complexRrStage__DOT__iqData[0U][1U] 
        = complexRrStage__DOT____Vlvbound_h51bf7b14__0[1U];
    vlSelfRef.__PVT__complexRrStage__DOT__iqData[0U][2U] 
        = complexRrStage__DOT____Vlvbound_h51bf7b14__0[2U];
    complexRrStage__DOT____Vlvbound_h4f6daee4__0 = 
        (7U & (vlSelfRef.__PVT__complexRrStage__DOT__iqData
               [0U][2U] >> 3U));
    vlSelfRef.__PVT__complexRrStage__DOT__mulOpInfo[0U] 
        = complexRrStage__DOT____Vlvbound_h4f6daee4__0;
    complexRrStage__DOT____Vlvbound_h79f92e5a__0 = 
        (0x1fffffU & ((vlSelfRef.__PVT__complexRrStage__DOT__iqData
                       [0U][1U] << 3U) | (vlSelfRef.__PVT__complexRrStage__DOT__iqData
                                          [0U][0U] 
                                          >> 0x1dU)));
    vlSelfRef.__PVT__complexRrStage__DOT__opSrc[0U] 
        = complexRrStage__DOT____Vlvbound_h79f92e5a__0;
    complexRrStage__DOT____Vlvbound_h9435fd27__0 = 
        (0xffU & (vlSelfRef.__PVT__complexRrStage__DOT__iqData
                  [0U][0U] >> 0x15U));
    vlSelfRef.__PVT__complexRrStage__DOT__opDst[0U] 
        = complexRrStage__DOT____Vlvbound_h9435fd27__0;
    complexRrStage__DOT____Vlvbound_h02c11781__0 = 
        (0x7fU & (vlSelfRef.__PVT__complexRrStage__DOT__opSrc
                  [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegNumA[0U] 
        = complexRrStage__DOT____Vlvbound_h02c11781__0;
    complexRrStage__DOT____Vlvbound_ha2604c03__0 = 
        (0x7fU & (vlSelfRef.__PVT__complexRrStage__DOT__opSrc
                  [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegNumB[0U] 
        = complexRrStage__DOT____Vlvbound_ha2604c03__0;
    complexRrStage__DOT____Vlvbound_h04fb3afd__0 = 
        (0x7fU & (vlSelfRef.__PVT__complexRrStage__DOT__opSrc
                  [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhySrcRegNumA[0U] 
        = complexRrStage__DOT____Vlvbound_h04fb3afd__0;
    complexRrStage__DOT____Vlvbound_h738e9198__0 = 
        (0x7fU & (vlSelfRef.__PVT__complexRrStage__DOT__opSrc
                  [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhySrcRegNumB[0U] 
        = complexRrStage__DOT____Vlvbound_h738e9198__0;
    complexRrStage__DOT____Vlvbound_h09578847__0 = 
        (1U & ((vlSelfRef.__PVT__complexRrStage__DOT__opDst
                [0U] >> 7U) & (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
                               [0U][2U] >> 0x13U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexWriteReg[0U] 
        = complexRrStage__DOT____Vlvbound_h09578847__0;
    complexRrStage__DOT____Vlvbound_h095fb107__0 = 
        (0x7fU & vlSelfRef.__PVT__complexRrStage__DOT__opDst
         [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexPhyDstRegNum[0U] 
        = complexRrStage__DOT____Vlvbound_h095fb107__0;
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexReadRegA[0U] = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexReadRegB[0U] = 1U;
    complexRrStage__DOT____Vlvbound_h022db8e7__0 = 
        (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
         [0U][2U] >> 0x14U);
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
        = ((0xfffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][5U]) | (0xffffffU & ((IData)(complexRrStage__DOT____Vlvbound_h022db8e7__0) 
                                      << 0xcU)));
    __Vfunc_SelectiveFlushDetector__551__opPtr = (vlSelfRef.__PVT__complexRrStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__551__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__551__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__551__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__551__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__551__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__551__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 1U;
                goto __Vlabel18;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__551__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__551__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__551__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__551__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__551__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 1U;
                    goto __Vlabel18;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 0U;
                    goto __Vlabel18;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__551__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__551__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__551__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__551__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__551__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 1U;
                    goto __Vlabel18;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__551__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__551__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__551__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 1U;
                    goto __Vlabel18;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 0U;
                    goto __Vlabel18;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 0U;
                goto __Vlabel18;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout = 0U;
        }
        __Vlabel18: ;
    }
    complexRrStage__DOT____Vlvbound_h046b483c__0 = vlSelfRef.__Vfunc_SelectiveFlushDetector__551__Vfuncout;
    vlSelfRef.__PVT__complexRrStage__DOT__flush[0U] 
        = complexRrStage__DOT____Vlvbound_h046b483c__0;
    complexRrStage__DOT____Vlvbound_ha5bb9e4e__0 = 
        (1U & ((~ ((((IData)(vlSelfRef.__PVT__complexRrStage__DOT__stall) 
                     | (IData)(vlSelfRef.__PVT__complexRrStage__DOT__clear)) 
                    | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                   | vlSelfRef.__PVT__complexRrStage__DOT__flush
                   [0U])) & (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
                             [0U][2U] >> 0x13U)));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
        = ((0xfff7ffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][5U]) | (0xffffffU & ((IData)(complexRrStage__DOT____Vlvbound_ha5bb9e4e__0) 
                                      << 0xbU)));
    complexRrStage__DOT____Vlvbound_ha630ed7f__0 = 
        (1U & (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
               [0U][2U] >> 0x12U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
        = ((0xfffbffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][5U]) | (0xffffffU & ((IData)(complexRrStage__DOT____Vlvbound_ha630ed7f__0) 
                                      << 0xaU)));
    if ((1U == (7U & vlSelfRef.__PVT__complexRrStage__DOT__iqData
                [0U][2U]))) {
        vlSelfRef.complexRrStage__DOT____Vlvbound_ha632c82d__0 
            = ((vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
                [0U][2U] >> 0x13U) & vlSelfRef.__PVT__complexRrStage__DOT__flush
               [0U]);
        vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
            = ((0xfffdffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
                [0U][5U]) | (0xffffffU & ((IData)(vlSelfRef.complexRrStage__DOT____Vlvbound_ha632c82d__0) 
                                          << 9U)));
    } else {
        vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
            = (0xfffdffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
               [0U][5U]);
    }
    complexRrStage__DOT____Vlvbound_h0a98cdc0__0 = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegDataA
        [0U];
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][1U] 
        = ((0x3fffffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][1U]) | ((IData)(complexRrStage__DOT____Vlvbound_h0a98cdc0__0) 
                         << 0x16U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][2U] 
        = ((0xff800000U & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][2U]) | (((IData)(complexRrStage__DOT____Vlvbound_h0a98cdc0__0) 
                          >> 0xaU) | ((IData)((complexRrStage__DOT____Vlvbound_h0a98cdc0__0 
                                               >> 0x20U)) 
                                      << 0x16U)));
    complexRrStage__DOT____Vlvbound_h0a827eb3__0 = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__complexSrcRegDataB
        [0U];
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][0U] 
        = ((0x1fffffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][0U]) | ((IData)(complexRrStage__DOT____Vlvbound_h0a827eb3__0) 
                         << 0x15U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][1U] 
        = ((0xffc00000U & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][1U]) | (((IData)(complexRrStage__DOT____Vlvbound_h0a827eb3__0) 
                          >> 0xbU) | ((IData)((complexRrStage__DOT____Vlvbound_h0a827eb3__0 
                                               >> 0x20U)) 
                                      << 0x15U)));
    complexRrStage__DOT____Vlvbound_hb64b5082__0[0U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
        [0U][0U];
    complexRrStage__DOT____Vlvbound_hb64b5082__0[1U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
        [0U][1U];
    complexRrStage__DOT____Vlvbound_hb64b5082__0[2U] 
        = (0x3ffffU & vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
           [0U][2U]);
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][2U] 
        = ((0x7fffffU & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][2U]) | (complexRrStage__DOT____Vlvbound_hb64b5082__0[0U] 
                         << 0x17U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][3U] 
        = ((complexRrStage__DOT____Vlvbound_hb64b5082__0[0U] 
            >> 9U) | (complexRrStage__DOT____Vlvbound_hb64b5082__0[1U] 
                      << 0x17U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][4U] 
        = ((complexRrStage__DOT____Vlvbound_hb64b5082__0[1U] 
            >> 9U) | (complexRrStage__DOT____Vlvbound_hb64b5082__0[2U] 
                      << 0x17U));
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][5U] 
        = ((0xfffe00U & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][5U]) | (0xffffffU & (complexRrStage__DOT____Vlvbound_hb64b5082__0[2U] 
                                      >> 9U)));
    complexRrStage__DOT____Vlvbound_h094f5bee__0 = 
        vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__complexCtrlOut
        [0U];
    vlSelfRef.__PVT__complexRrStage__DOT__nextStage[0U][0U] 
        = ((0xffe00000U & vlSelfRef.__PVT__complexRrStage__DOT__nextStage
            [0U][0U]) | complexRrStage__DOT____Vlvbound_h094f5bee__0);
    vlSelfRef.__PVT__complexRrStage__DOT__unnamedblk3__DOT__i = 1U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__complexRrStageIF.__PVT__nextStage[0U][5U] 
        = vlSelfRef.__PVT__complexRrStage__DOT__nextStage
        [0U][5U];
    complexRrStage__DOT____Vlvbound_h31a1399f__0 = 
        (1U & (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
               [0U][2U] >> 0x13U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
            [0U]) | ((IData)(complexRrStage__DOT____Vlvbound_h31a1399f__0) 
                     << 0xdU));
    complexRrStage__DOT____Vlvbound_h31a147f0__0 = 
        vlSelfRef.__PVT__complexRrStage__DOT__flush
        [0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
            [0U]) | ((IData)(complexRrStage__DOT____Vlvbound_h31a147f0__0) 
                     << 0xcU));
    complexRrStage__DOT____Vlvbound_hbb48d9af__0 = 
        (vlSelfRef.__PVT__complexRrStage__DOT__pipeReg
         [0U][2U] >> 0x14U);
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__complexRrReg
            [0U]) | (IData)(complexRrStage__DOT____Vlvbound_hbb48d9af__0));
    vlSelfRef.__PVT__complexRrStage__DOT__unnamedblk4__DOT__i = 1U;
}

VL_INLINE_OPT void VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__1(VSMT_RTL_Testbench_Core* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+              VSMT_RTL_Testbench_Core___act_comb__TOP__SMT_RTL_Testbench__core__1\n"); );
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Init
    IData/*31:0*/ __Vfunc_ToAddrFromPC__532__Vfuncout;
    __Vfunc_ToAddrFromPC__532__Vfuncout = 0;
    IData/*19:0*/ __Vfunc_ToAddrFromPC__532__pc;
    __Vfunc_ToAddrFromPC__532__pc = 0;
    IData/*31:0*/ __Vfunc_RISCV_OpImm__533__Vfuncout;
    __Vfunc_RISCV_OpImm__533__Vfuncout = 0;
    IData/*29:0*/ __Vfunc_RISCV_OpImm__533__intOperandImm;
    __Vfunc_RISCV_OpImm__533__intOperandImm = 0;
    IData/*31:0*/ __Vfunc_RISCV_OpImm__533__result;
    __Vfunc_RISCV_OpImm__533__result = 0;
    CData/*1:0*/ __Vfunc_SelectOperandIntReg__534__opType;
    __Vfunc_SelectOperandIntReg__534__opType = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__534__regV;
    __Vfunc_SelectOperandIntReg__534__regV = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__534__immV;
    __Vfunc_SelectOperandIntReg__534__immV = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__534__pcV;
    __Vfunc_SelectOperandIntReg__534__pcV = 0;
    CData/*1:0*/ __Vfunc_SelectOperandIntReg__535__opType;
    __Vfunc_SelectOperandIntReg__535__opType = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__535__regV;
    __Vfunc_SelectOperandIntReg__535__regV = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__535__immV;
    __Vfunc_SelectOperandIntReg__535__immV = 0;
    IData/*31:0*/ __Vfunc_SelectOperandIntReg__535__pcV;
    __Vfunc_SelectOperandIntReg__535__pcV = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__536__detectRange;
    __Vfunc_SelectiveFlushDetector__536__detectRange = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__536__headPtr;
    __Vfunc_SelectiveFlushDetector__536__headPtr = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__536__tailPtr;
    __Vfunc_SelectiveFlushDetector__536__tailPtr = 0;
    CData/*0:0*/ __Vfunc_SelectiveFlushDetector__536__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__536__flushAllInsns = 0;
    CData/*5:0*/ __Vfunc_SelectiveFlushDetector__536__opPtr;
    __Vfunc_SelectiveFlushDetector__536__opPtr = 0;
    // Body
    vlSelfRef.__PVT__intRrStage__DOT__stall = (1U & 
                                               ((IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd) 
                                                >> 1U));
    vlSelfRef.__PVT__intRrStage__DOT__clear = (1U & (IData)(vlSymsp->TOP__SMT_RTL_Testbench__core__ctrlIF.__PVT__backEnd));
    vlSelfRef.__PVT__intRrStage__DOT__iqData[0U][0U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [0U][0U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[0U][1U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [0U][1U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[0U][2U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [0U][2U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[0U][3U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [0U][3U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[0U][4U] 
        = (0x7ffU & vlSelfRef.__PVT__intRrStage__DOT__pipeReg
           [0U][4U]);
    vlSelfRef.__PVT__intRrStage__DOT__intSubInfo[0U] 
        = (0x1ffffffffffffffULL & (((QData)((IData)(
                                                    vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                    [0U][3U])) 
                                    << 0x1aU) | ((QData)((IData)(
                                                                 vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                                 [0U][2U])) 
                                                 >> 6U)));
    vlSelfRef.__PVT__intRrStage__DOT__opSrc[0U] = (0x1fffffU 
                                                   & ((vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                       [0U][1U] 
                                                       << 3U) 
                                                      | (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                         [0U][0U] 
                                                         >> 0x1dU)));
    vlSelfRef.__PVT__intRrStage__DOT__opDst[0U] = (0xffU 
                                                   & (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                      [0U][0U] 
                                                      >> 0x15U));
    __Vfunc_ToAddrFromPC__532__pc = (0xfffffU & (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                 [0U][0U] 
                                                 >> 1U));
    __Vfunc_ToAddrFromPC__532__Vfuncout = ((0x80000000U 
                                            & (__Vfunc_ToAddrFromPC__532__pc 
                                               << 0xdU)) 
                                           | (0x3ffffU 
                                              & __Vfunc_ToAddrFromPC__532__pc));
    vlSelfRef.__PVT__intRrStage__DOT__pc[0U] = __Vfunc_ToAddrFromPC__532__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumA[0U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumB[0U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumA[0U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [0U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumB[0U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [0U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intWriteReg[0U] 
        = (1U & ((vlSelfRef.__PVT__intRrStage__DOT__opDst
                  [0U] >> 7U) & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                 [0U][4U] >> 0xbU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhyDstRegNum[0U] 
        = (0x7fU & vlSelfRef.__PVT__intRrStage__DOT__opDst
           [0U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegA[0U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                [0U] >> 0x37U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegB[0U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                [0U] >> 0x35U))));
    __Vfunc_RISCV_OpImm__533__intOperandImm = (0x3fffffffU 
                                               & (IData)(
                                                         (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                          [0U] 
                                                          >> 0x12U)));
    __Vfunc_RISCV_OpImm__533__result = ((3U == (3U 
                                                & __Vfunc_RISCV_OpImm__533__intOperandImm))
                                         ? (0xfffff000U 
                                            & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                               << 0xaU))
                                         : (((- (IData)(
                                                        (1U 
                                                         & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                                            >> 0x15U)))) 
                                             << 0x14U) 
                                            | (0xfffffU 
                                               & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                                  >> 2U))));
    __Vfunc_RISCV_OpImm__533__Vfuncout = __Vfunc_RISCV_OpImm__533__result;
    vlSelfRef.__PVT__intRrStage__DOT__immOut[0U] = __Vfunc_RISCV_OpImm__533__Vfuncout;
    __Vfunc_SelectOperandIntReg__534__pcV = vlSelfRef.__PVT__intRrStage__DOT__pc
        [0U];
    __Vfunc_SelectOperandIntReg__534__immV = vlSelfRef.__PVT__intRrStage__DOT__immOut
        [0U];
    __Vfunc_SelectOperandIntReg__534__regV = (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataA
                                                     [0U]);
    __Vfunc_SelectOperandIntReg__534__opType = (3U 
                                                & (IData)(
                                                          (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                           [0U] 
                                                           >> 0x37U)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperandIntReg__534__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperandIntReg__534__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__pcV;
            goto __Vlabel19;
        } else {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__regV;
            goto __Vlabel19;
        }
        __Vlabel19: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__operandA[0U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__intRrStage__DOT__operandA
            [0U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout)));
    __Vfunc_SelectOperandIntReg__535__pcV = vlSelfRef.__PVT__intRrStage__DOT__pc
        [0U];
    __Vfunc_SelectOperandIntReg__535__immV = vlSelfRef.__PVT__intRrStage__DOT__immOut
        [0U];
    __Vfunc_SelectOperandIntReg__535__regV = (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataB
                                                     [0U]);
    __Vfunc_SelectOperandIntReg__535__opType = (3U 
                                                & (IData)(
                                                          (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                           [0U] 
                                                           >> 0x35U)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperandIntReg__535__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperandIntReg__535__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__pcV;
            goto __Vlabel20;
        } else {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__regV;
            goto __Vlabel20;
        }
        __Vlabel20: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__operandB[0U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__intRrStage__DOT__operandB
            [0U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout)));
    vlSelfRef.__PVT__intRrStage__DOT__operandA[0U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__intRrStage__DOT__operandA
            [0U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                            [0U] 
                                                            >> 0x37U)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataA
                                                       [0U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__intRrStage__DOT__operandB[0U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__intRrStage__DOT__operandB
            [0U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                            [0U] 
                                                            >> 0x35U)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataB
                                                       [0U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][7U] 
        = ((7U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][7U]) | (0x7ff8U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                    [0U][4U] >> 9U)));
    __Vfunc_SelectiveFlushDetector__536__opPtr = (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                  [0U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__536__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__536__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__536__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__536__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__536__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__536__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                goto __Vlabel21;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__536__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel21;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                    goto __Vlabel21;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__536__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel21;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel21;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                    goto __Vlabel21;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                goto __Vlabel21;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
        }
        __Vlabel21: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__flush[0U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout;
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][7U] 
        = ((0x7ffbU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][7U]) | (4U & (((~ ((((IData)(vlSelfRef.__PVT__intRrStage__DOT__stall) 
                                      | (IData)(vlSelfRef.__PVT__intRrStage__DOT__clear)) 
                                     | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                    | vlSelfRef.__PVT__intRrStage__DOT__flush
                                    [0U])) << 2U) & 
                               (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                [0U][4U] >> 9U))));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][1U] 
        = ((0x3fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][1U]) | ((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandA
                                 [0U]) << 0x16U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][2U] 
        = ((0xff800000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][2U]) | (((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandA
                                  [0U]) >> 0xaU) | 
                         ((IData)((vlSelfRef.__PVT__intRrStage__DOT__operandA
                                   [0U] >> 0x20U)) 
                          << 0x16U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][0U] 
        = ((0x1fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][0U]) | ((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandB
                                 [0U]) << 0x15U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][1U] 
        = ((0xffc00000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][1U]) | (((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandB
                                  [0U]) >> 0xbU) | 
                         ((IData)((vlSelfRef.__PVT__intRrStage__DOT__operandB
                                   [0U] >> 0x20U)) 
                          << 0x15U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][2U] 
        = ((0x7fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][2U]) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                         [0U][0U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][3U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [0U][0U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][1U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][4U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [0U][1U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][2U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][5U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [0U][2U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][3U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][6U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [0U][3U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][4U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][7U] 
        = ((0x7ffcU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][7U]) | (3U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][4U] >> 9U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[0U][0U] 
        = ((0xffe00000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [0U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
           [0U]);
    vlSelfRef.__PVT__intRrStage__DOT__iqData[1U][0U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [1U][0U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[1U][1U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [1U][1U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[1U][2U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [1U][2U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[1U][3U] 
        = vlSelfRef.__PVT__intRrStage__DOT__pipeReg
        [1U][3U];
    vlSelfRef.__PVT__intRrStage__DOT__iqData[1U][4U] 
        = (0x7ffU & vlSelfRef.__PVT__intRrStage__DOT__pipeReg
           [1U][4U]);
    vlSelfRef.__PVT__intRrStage__DOT__intSubInfo[1U] 
        = (0x1ffffffffffffffULL & (((QData)((IData)(
                                                    vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                    [1U][3U])) 
                                    << 0x1aU) | ((QData)((IData)(
                                                                 vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                                 [1U][2U])) 
                                                 >> 6U)));
    vlSelfRef.__PVT__intRrStage__DOT__opSrc[1U] = (0x1fffffU 
                                                   & ((vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                       [1U][1U] 
                                                       << 3U) 
                                                      | (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                         [1U][0U] 
                                                         >> 0x1dU)));
    vlSelfRef.__PVT__intRrStage__DOT__opDst[1U] = (0xffU 
                                                   & (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                      [1U][0U] 
                                                      >> 0x15U));
    __Vfunc_ToAddrFromPC__532__pc = (0xfffffU & (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                 [1U][0U] 
                                                 >> 1U));
    __Vfunc_ToAddrFromPC__532__Vfuncout = ((0x80000000U 
                                            & (__Vfunc_ToAddrFromPC__532__pc 
                                               << 0xdU)) 
                                           | (0x3ffffU 
                                              & __Vfunc_ToAddrFromPC__532__pc));
    vlSelfRef.__PVT__intRrStage__DOT__pc[1U] = __Vfunc_ToAddrFromPC__532__Vfuncout;
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumA[1U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [1U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegNumB[1U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [1U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumA[1U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [1U] >> 0xeU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhySrcRegNumB[1U] 
        = (0x7fU & (vlSelfRef.__PVT__intRrStage__DOT__opSrc
                    [1U] >> 7U));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intWriteReg[1U] 
        = (1U & ((vlSelfRef.__PVT__intRrStage__DOT__opDst
                  [1U] >> 7U) & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                 [1U][4U] >> 0xbU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intPhyDstRegNum[1U] 
        = (0x7fU & vlSelfRef.__PVT__intRrStage__DOT__opDst
           [1U]);
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegA[1U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                [1U] >> 0x37U))));
    vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intReadRegB[1U] 
        = (0U == (3U & (IData)((vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                [1U] >> 0x35U))));
    __Vfunc_RISCV_OpImm__533__intOperandImm = (0x3fffffffU 
                                               & (IData)(
                                                         (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                          [1U] 
                                                          >> 0x12U)));
    __Vfunc_RISCV_OpImm__533__result = ((3U == (3U 
                                                & __Vfunc_RISCV_OpImm__533__intOperandImm))
                                         ? (0xfffff000U 
                                            & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                               << 0xaU))
                                         : (((- (IData)(
                                                        (1U 
                                                         & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                                            >> 0x15U)))) 
                                             << 0x14U) 
                                            | (0xfffffU 
                                               & (__Vfunc_RISCV_OpImm__533__intOperandImm 
                                                  >> 2U))));
    __Vfunc_RISCV_OpImm__533__Vfuncout = __Vfunc_RISCV_OpImm__533__result;
    vlSelfRef.__PVT__intRrStage__DOT__immOut[1U] = __Vfunc_RISCV_OpImm__533__Vfuncout;
    __Vfunc_SelectOperandIntReg__534__pcV = vlSelfRef.__PVT__intRrStage__DOT__pc
        [1U];
    __Vfunc_SelectOperandIntReg__534__immV = vlSelfRef.__PVT__intRrStage__DOT__immOut
        [1U];
    __Vfunc_SelectOperandIntReg__534__regV = (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataA
                                                     [1U]);
    __Vfunc_SelectOperandIntReg__534__opType = (3U 
                                                & (IData)(
                                                          (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                           [1U] 
                                                           >> 0x37U)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperandIntReg__534__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperandIntReg__534__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__pcV;
            goto __Vlabel22;
        } else {
            vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout 
                = __Vfunc_SelectOperandIntReg__534__regV;
            goto __Vlabel22;
        }
        __Vlabel22: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__operandA[1U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__intRrStage__DOT__operandA
            [1U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperandIntReg__534__Vfuncout)));
    __Vfunc_SelectOperandIntReg__535__pcV = vlSelfRef.__PVT__intRrStage__DOT__pc
        [1U];
    __Vfunc_SelectOperandIntReg__535__immV = vlSelfRef.__PVT__intRrStage__DOT__immOut
        [1U];
    __Vfunc_SelectOperandIntReg__535__regV = (IData)(
                                                     vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataB
                                                     [1U]);
    __Vfunc_SelectOperandIntReg__535__opType = (3U 
                                                & (IData)(
                                                          (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                           [1U] 
                                                           >> 0x35U)));
    {
        if ((1U == (IData)(__Vfunc_SelectOperandIntReg__535__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__immV;
        } else if ((2U == (IData)(__Vfunc_SelectOperandIntReg__535__opType))) {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__pcV;
            goto __Vlabel23;
        } else {
            vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout 
                = __Vfunc_SelectOperandIntReg__535__regV;
            goto __Vlabel23;
        }
        __Vlabel23: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__operandB[1U] 
        = ((0x100000000ULL & vlSelfRef.__PVT__intRrStage__DOT__operandB
            [1U]) | (IData)((IData)(vlSelfRef.__Vfunc_SelectOperandIntReg__535__Vfuncout)));
    vlSelfRef.__PVT__intRrStage__DOT__operandA[1U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__intRrStage__DOT__operandA
            [1U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                            [1U] 
                                                            >> 0x37U)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataA
                                                       [1U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__intRrStage__DOT__operandB[1U] 
        = ((0xffffffffULL & vlSelfRef.__PVT__intRrStage__DOT__operandB
            [1U]) | ((QData)((IData)((1U & ((0U != 
                                             (3U & (IData)(
                                                           (vlSelfRef.__PVT__intRrStage__DOT__intSubInfo
                                                            [1U] 
                                                            >> 0x35U)))) 
                                            | (IData)(
                                                      (vlSymsp->TOP__SMT_RTL_Testbench__core__registerFileIF.__PVT__intSrcRegDataB
                                                       [1U] 
                                                       >> 0x20U)))))) 
                     << 0x20U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][7U] 
        = ((7U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][7U]) | (0x7ff8U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                    [1U][4U] >> 9U)));
    __Vfunc_SelectiveFlushDetector__536__opPtr = (vlSelfRef.__PVT__intRrStage__DOT__iqData
                                                  [1U][1U] 
                                                  >> 0x1aU);
    __Vfunc_SelectiveFlushDetector__536__flushAllInsns 
        = vlSymsp->TOP__SMT_RTL_Testbench__core__recoveryManagerIF.__PVT__flushAllInsns;
    __Vfunc_SelectiveFlushDetector__536__tailPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 4U));
    __Vfunc_SelectiveFlushDetector__536__headPtr = 
        (0x3fU & (vlSelfRef.__PVT__recoveryManager__DOT__regState[0U] 
                  >> 0xaU));
    __Vfunc_SelectiveFlushDetector__536__detectRange 
        = (1U == (3U & (vlSelfRef.__PVT__recoveryManager__DOT__regState[3U] 
                        >> 0x15U)));
    {
        if (__Vfunc_SelectiveFlushDetector__536__detectRange) {
            if (__Vfunc_SelectiveFlushDetector__536__flushAllInsns) {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                goto __Vlabel24;
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__536__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr) 
                           >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                        < (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel24;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                    goto __Vlabel24;
                }
            } else if (((IData)(__Vfunc_SelectiveFlushDetector__536__detectRange) 
                        & ((IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr) 
                           < (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)))) {
                if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                      >= (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                     & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                        > (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel24;
                } else if ((((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                             < (IData)(__Vfunc_SelectiveFlushDetector__536__headPtr)) 
                            & ((IData)(__Vfunc_SelectiveFlushDetector__536__opPtr) 
                               < (IData)(__Vfunc_SelectiveFlushDetector__536__tailPtr)))) {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 1U;
                    goto __Vlabel24;
                } else {
                    vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                    goto __Vlabel24;
                }
            } else {
                vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
                goto __Vlabel24;
            }
        } else {
            vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout = 0U;
        }
        __Vlabel24: ;
    }
    vlSelfRef.__PVT__intRrStage__DOT__flush[1U] = vlSelfRef.__Vfunc_SelectiveFlushDetector__536__Vfuncout;
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][7U] 
        = ((0x7ffbU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][7U]) | (4U & (((~ ((((IData)(vlSelfRef.__PVT__intRrStage__DOT__stall) 
                                      | (IData)(vlSelfRef.__PVT__intRrStage__DOT__clear)) 
                                     | (IData)(vlSymsp->TOP__SMT_RTL_Testbench.__PVT__rst)) 
                                    | vlSelfRef.__PVT__intRrStage__DOT__flush
                                    [1U])) << 2U) & 
                               (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                [1U][4U] >> 9U))));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][1U] 
        = ((0x3fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][1U]) | ((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandA
                                 [1U]) << 0x16U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][2U] 
        = ((0xff800000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][2U]) | (((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandA
                                  [1U]) >> 0xaU) | 
                         ((IData)((vlSelfRef.__PVT__intRrStage__DOT__operandA
                                   [1U] >> 0x20U)) 
                          << 0x16U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][0U] 
        = ((0x1fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][0U]) | ((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandB
                                 [1U]) << 0x15U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][1U] 
        = ((0xffc00000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][1U]) | (((IData)(vlSelfRef.__PVT__intRrStage__DOT__operandB
                                  [1U]) >> 0xbU) | 
                         ((IData)((vlSelfRef.__PVT__intRrStage__DOT__operandB
                                   [1U] >> 0x20U)) 
                          << 0x15U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][2U] 
        = ((0x7fffffU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][2U]) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                         [1U][0U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][3U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [1U][0U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][1U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][4U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [1U][1U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][2U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][5U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [1U][2U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][3U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][6U] 
        = ((vlSelfRef.__PVT__intRrStage__DOT__pipeReg
            [1U][3U] >> 9U) | (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][4U] << 0x17U));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][7U] 
        = ((0x7ffcU & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][7U]) | (3U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][4U] >> 9U)));
    vlSelfRef.__PVT__intRrStage__DOT__nextStage[1U][0U] 
        = ((0xffe00000U & vlSelfRef.__PVT__intRrStage__DOT__nextStage
            [1U][0U]) | vlSymsp->TOP__SMT_RTL_Testbench__core__bypassNetworkIF.__PVT__intCtrlOut
           [1U]);
    vlSelfRef.__PVT__intRrStage__DOT__unnamedblk3__DOT__i = 2U;
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][0U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][1U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][2U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][3U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][4U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][5U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][6U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[0U][7U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [0U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][0U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][0U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][1U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][1U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][2U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][2U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][3U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][3U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][4U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][4U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][5U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][5U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][6U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][6U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__intRrStageIF.__PVT__nextStage[1U][7U] 
        = vlSelfRef.__PVT__intRrStage__DOT__nextStage
        [1U][7U];
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[0U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [0U]) | (0x2000U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                [0U][4U] << 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[0U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [0U]) | (vlSelfRef.__PVT__intRrStage__DOT__flush
                     [0U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[0U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [0U]) | (0xfffU & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [0U][4U] >> 0xcU)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[1U] 
        = ((0x1fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [1U]) | (0x2000U & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                                [1U][4U] << 2U)));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[1U] 
        = ((0x2fffU & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [1U]) | (vlSelfRef.__PVT__intRrStage__DOT__flush
                     [1U] << 0xcU));
    vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg[1U] 
        = ((0x3000U & vlSymsp->TOP__SMT_RTL_Testbench__core__debugIF.__PVT__intRrReg
            [1U]) | (0xfffU & (vlSelfRef.__PVT__intRrStage__DOT__pipeReg
                               [1U][4U] >> 0xcU)));
    vlSelfRef.__PVT__intRrStage__DOT__unnamedblk4__DOT__i = 2U;
}
