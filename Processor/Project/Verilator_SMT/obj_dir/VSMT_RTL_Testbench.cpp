// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Model implementation (design independent parts)

#include "VSMT_RTL_Testbench__pch.h"
#include "verilated_vcd_c.h"

//============================================================
// Constructors

VSMT_RTL_Testbench::VSMT_RTL_Testbench(VerilatedContext* _vcontextp__, const char* _vcname__)
    : VerilatedModel{*_vcontextp__}
    , vlSymsp{new VSMT_RTL_Testbench__Syms(contextp(), _vcname__, this)}
    , SMT_RTL_Testbench{vlSymsp->TOP.SMT_RTL_Testbench}
    , __PVT____024unit{vlSymsp->TOP.__PVT____024unit}
    , MemoryTypes{vlSymsp->TOP.MemoryTypes}
    , __PVT__DumperTypes{vlSymsp->TOP.__PVT__DumperTypes}
    , DumperTypes__03a__03aKanataDumper__Vclpkg{vlSymsp->TOP.DumperTypes__03a__03aKanataDumper__Vclpkg}
    , DumperTypes__03a__03aSerialDumper__Vclpkg{vlSymsp->TOP.DumperTypes__03a__03aSerialDumper__Vclpkg}
    , DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg{vlSymsp->TOP.DumperTypes__03a__03aRegisterFileHexDumper__Vclpkg}
    , DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg{vlSymsp->TOP.DumperTypes__03a__03aRegisterFileCSV_Dumper__Vclpkg}
    , rootp{&(vlSymsp->TOP)}
{
    // Register model with the context
    contextp()->addModel(this);
    contextp()->traceBaseModelCbAdd(
        [this](VerilatedTraceBaseC* tfp, int levels, int options) { traceBaseModel(tfp, levels, options); });
}

VSMT_RTL_Testbench::VSMT_RTL_Testbench(const char* _vcname__)
    : VSMT_RTL_Testbench(Verilated::threadContextp(), _vcname__)
{
}

//============================================================
// Destructor

VSMT_RTL_Testbench::~VSMT_RTL_Testbench() {
    delete vlSymsp;
}

//============================================================
// Evaluation function

#ifdef VL_DEBUG
void VSMT_RTL_Testbench___024root___eval_debug_assertions(VSMT_RTL_Testbench___024root* vlSelf);
#endif  // VL_DEBUG
void VSMT_RTL_Testbench___024root___eval_static(VSMT_RTL_Testbench___024root* vlSelf);
void VSMT_RTL_Testbench___024root___eval_initial(VSMT_RTL_Testbench___024root* vlSelf);
void VSMT_RTL_Testbench___024root___eval_settle(VSMT_RTL_Testbench___024root* vlSelf);
void VSMT_RTL_Testbench___024root___eval(VSMT_RTL_Testbench___024root* vlSelf);

void VSMT_RTL_Testbench::eval_step() {
    VL_DEBUG_IF(VL_DBG_MSGF("+++++TOP Evaluate VSMT_RTL_Testbench::eval_step\n"); );
#ifdef VL_DEBUG
    // Debug assertions
    VSMT_RTL_Testbench___024root___eval_debug_assertions(&(vlSymsp->TOP));
#endif  // VL_DEBUG
    vlSymsp->__Vm_activity = true;
    vlSymsp->__Vm_deleter.deleteAll();
    if (VL_UNLIKELY(!vlSymsp->__Vm_didInit)) {
        vlSymsp->__Vm_didInit = true;
        VL_DEBUG_IF(VL_DBG_MSGF("+ Initial\n"););
        VSMT_RTL_Testbench___024root___eval_static(&(vlSymsp->TOP));
        VSMT_RTL_Testbench___024root___eval_initial(&(vlSymsp->TOP));
        VSMT_RTL_Testbench___024root___eval_settle(&(vlSymsp->TOP));
    }
    VL_DEBUG_IF(VL_DBG_MSGF("+ Eval\n"););
    VSMT_RTL_Testbench___024root___eval(&(vlSymsp->TOP));
    // Evaluate cleanup
    Verilated::endOfEval(vlSymsp->__Vm_evalMsgQp);
}

void VSMT_RTL_Testbench::eval_end_step() {
    VL_DEBUG_IF(VL_DBG_MSGF("+eval_end_step VSMT_RTL_Testbench::eval_end_step\n"); );
#ifdef VM_TRACE
    // Tracing
    if (VL_UNLIKELY(vlSymsp->__Vm_dumping)) vlSymsp->_traceDump();
#endif  // VM_TRACE
}

//============================================================
// Events and timing
bool VSMT_RTL_Testbench::eventsPending() { return !vlSymsp->TOP.__VdlySched.empty(); }

uint64_t VSMT_RTL_Testbench::nextTimeSlot() { return vlSymsp->TOP.__VdlySched.nextTimeSlot(); }

//============================================================
// Utilities

const char* VSMT_RTL_Testbench::name() const {
    return vlSymsp->name();
}

//============================================================
// Invoke final blocks

void VSMT_RTL_Testbench___024root___eval_final(VSMT_RTL_Testbench___024root* vlSelf);

VL_ATTR_COLD void VSMT_RTL_Testbench::final() {
    VSMT_RTL_Testbench___024root___eval_final(&(vlSymsp->TOP));
}

//============================================================
// Implementations of abstract methods from VerilatedModel

const char* VSMT_RTL_Testbench::hierName() const { return vlSymsp->name(); }
const char* VSMT_RTL_Testbench::modelName() const { return "VSMT_RTL_Testbench"; }
unsigned VSMT_RTL_Testbench::threads() const { return 1; }
void VSMT_RTL_Testbench::prepareClone() const { contextp()->prepareClone(); }
void VSMT_RTL_Testbench::atClone() const {
    contextp()->threadPoolpOnClone();
}
std::unique_ptr<VerilatedTraceConfig> VSMT_RTL_Testbench::traceConfig() const {
    return std::unique_ptr<VerilatedTraceConfig>{new VerilatedTraceConfig{false, false, false}};
};

//============================================================
// Trace configuration

void VSMT_RTL_Testbench___024root__trace_decl_types(VerilatedVcd* tracep);

void VSMT_RTL_Testbench___024root__trace_init_top(VSMT_RTL_Testbench___024root* vlSelf, VerilatedVcd* tracep);

VL_ATTR_COLD static void trace_init(void* voidSelf, VerilatedVcd* tracep, uint32_t code) {
    // Callback from tracep->open()
    VSMT_RTL_Testbench___024root* const __restrict vlSelf VL_ATTR_UNUSED = static_cast<VSMT_RTL_Testbench___024root*>(voidSelf);
    VSMT_RTL_Testbench__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    if (!vlSymsp->_vm_contextp__->calcUnusedSigs()) {
        VL_FATAL_MT(__FILE__, __LINE__, __FILE__,
            "Turning on wave traces requires Verilated::traceEverOn(true) call before time 0.");
    }
    vlSymsp->__Vm_baseCode = code;
    tracep->pushPrefix(std::string{vlSymsp->name()}, VerilatedTracePrefixType::SCOPE_MODULE);
    VSMT_RTL_Testbench___024root__trace_decl_types(tracep);
    VSMT_RTL_Testbench___024root__trace_init_top(vlSelf, tracep);
    tracep->popPrefix();
}

VL_ATTR_COLD void VSMT_RTL_Testbench___024root__trace_register(VSMT_RTL_Testbench___024root* vlSelf, VerilatedVcd* tracep);

VL_ATTR_COLD void VSMT_RTL_Testbench::traceBaseModel(VerilatedTraceBaseC* tfp, int levels, int options) {
    (void)levels; (void)options;
    VerilatedVcdC* const stfp = dynamic_cast<VerilatedVcdC*>(tfp);
    if (VL_UNLIKELY(!stfp)) {
        vl_fatal(__FILE__, __LINE__, __FILE__,"'VSMT_RTL_Testbench::trace()' called on non-VerilatedVcdC object;"
            " use --trace-fst with VerilatedFst object, and --trace-vcd with VerilatedVcd object");
    }
    stfp->spTrace()->addModel(this);
    stfp->spTrace()->addInitCb(&trace_init, &(vlSymsp->TOP));
    VSMT_RTL_Testbench___024root__trace_register(&(vlSymsp->TOP), stfp->spTrace());
}
