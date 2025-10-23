// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See VSMT_RTL_Testbench.h for the primary calling header

#ifndef VERILATED_VSMT_RTL_TESTBENCH_WAKEUPLOGIC_H_
#define VERILATED_VSMT_RTL_TESTBENCH_WAKEUPLOGIC_H_  // guard

#include "verilated.h"
#include "verilated_timing.h"
class VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80;


class VSMT_RTL_Testbench__Syms;

class alignas(VL_CACHE_LINE_BYTES) VSMT_RTL_Testbench_WakeupLogic final : public VerilatedModule {
  public:
    // CELLS
    VSMT_RTL_Testbench_ReadyBitTable__S3_R7_E80* regReadyBitTbl;

    // DESIGN SPECIFIC STATE
    IData/*23:0*/ __PVT__dispatchedSrcRegPtr;
    SData/*15:0*/ __PVT__storeBitVector;
    SData/*15:0*/ __PVT__storeBitVectorReg;
    SData/*15:0*/ __PVT__notIssued;
    VlWide<8>/*255:0*/ __PVT__producerMatrix__DOT__nextMatrix;
    VlWide<8>/*255:0*/ __PVT__producerMatrix__DOT__matrix;
    IData/*31:0*/ __PVT__producerMatrix__DOT__dispatchVector;
    SData/*15:0*/ __PVT__producerMatrix__DOT__wakeupVector;
    VlUnpacked<VlUnpacked<CData/*0:0*/, 3>, 2> __PVT__dispatchedSrcRegValid;
    VlUnpacked<VlUnpacked<CData/*6:0*/, 3>, 2> __PVT__dispatchedSrcRegNum;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dispatchedDstRegValid;
    VlUnpacked<CData/*6:0*/, 2> __PVT__dispatchedDstRegNum;
    VlUnpacked<CData/*0:0*/, 5> __PVT__wakeupDstRegValid;
    VlUnpacked<CData/*6:0*/, 5> __PVT__wakeupDstRegNum;
    VlUnpacked<VlUnpacked<CData/*0:0*/, 3>, 2> __PVT__dispatchedSrcRegReady;
    VlUnpacked<SData/*15:0*/, 6> __PVT__wakeupDstVector;
    VlUnpacked<CData/*0:0*/, 16> __PVT__opMatrixReady;
    VlUnpacked<SData/*15:0*/, 2> __PVT__dependStoreBitVector;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dispatchStore;
    VlUnpacked<CData/*0:0*/, 2> __PVT__dispatchLoad;
    VlUnpacked<CData/*0:0*/, 2> __PVT__memDependencyPred;
    VlUnpacked<VlUnpacked<CData/*6:0*/, 3>, 2> __Vcellinp__regReadyBitTbl__dispatchedSrcRegNum;
    VlUnpacked<CData/*6:0*/, 2> __Vcellinp__regReadyBitTbl__dispatchedDstRegNum;
    VlUnpacked<CData/*0:0*/, 2> __Vcellinp__regReadyBitTbl__dispatch;
    VlUnpacked<CData/*6:0*/, 5> __Vcellinp__regReadyBitTbl__wakeupDstRegNum;
    VlUnpacked<CData/*0:0*/, 5> __Vcellinp__regReadyBitTbl__wakeup;
    VlUnpacked<CData/*3:0*/, 2> __Vcellinp__producerMatrix__dispatchPtr;
    VlUnpacked<CData/*0:0*/, 2> __Vcellinp__producerMatrix__dispatch;

    // INTERNAL VARIABLES
    VSMT_RTL_Testbench__Syms* const vlSymsp;

    // CONSTRUCTORS
    VSMT_RTL_Testbench_WakeupLogic(VSMT_RTL_Testbench__Syms* symsp, const char* v__name);
    ~VSMT_RTL_Testbench_WakeupLogic();
    VL_UNCOPYABLE(VSMT_RTL_Testbench_WakeupLogic);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
};


#endif  // guard
