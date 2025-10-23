# Verilated -*- Makefile -*-
# DESCRIPTION: Verilator output: Make include file with class lists
#
# This file lists generated Verilated files, for including in higher level makefiles.
# See VSMT_RTL_Testbench.mk for the caller.

### Switches...
# C11 constructs required?  0/1 (always on now)
VM_C11 = 1
# Timing enabled?  0/1
VM_TIMING = 1
# Coverage output mode?  0/1 (from --coverage)
VM_COVERAGE = 0
# Parallel builds?  0/1 (from --output-split)
VM_PARALLEL_BUILDS = 1
# Tracing output mode?  0/1 (from --trace-fst/--trace-saif/--trace-vcd)
VM_TRACE = 1
# Tracing output mode in FST format?  0/1 (from --trace-fst)
VM_TRACE_FST = 0
# Tracing output mode in SAIF format?  0/1 (from --trace-saif)
VM_TRACE_SAIF = 0
# Tracing output mode in VCD format?  0/1 (from --trace-vcd)
VM_TRACE_VCD = 1

### Object file lists...
# Generated module classes, fast-path, compile with highest optimization
VM_CLASSES_FAST += \
	VSMT_RTL_Testbench_vm_classes_0 \
	VSMT_RTL_Testbench_Core__DepSet_hf5efef52__6 \
	VSMT_RTL_Testbench_vm_classes_1 \
	VSMT_RTL_Testbench_vm_classes_2 \
	VSMT_RTL_Testbench_vm_classes_3 \
	VSMT_RTL_Testbench_vm_classes_4 \
	VSMT_RTL_Testbench_vm_classes_5 \
	VSMT_RTL_Testbench_vm_classes_6 \

# Generated module classes, non-fast-path, compile with low/medium optimization
VM_CLASSES_SLOW += \
	VSMT_RTL_Testbench_vm_classes_Slow_0 \
	VSMT_RTL_Testbench_Core__DepSet_h0fcabeb5__0__Slow \
	VSMT_RTL_Testbench_Core__DepSet_hf5efef52__0__Slow \
	VSMT_RTL_Testbench_Core__DepSet_hf5efef52__1__Slow \
	VSMT_RTL_Testbench_Core__DepSet_hf5efef52__2__Slow \
	VSMT_RTL_Testbench_Core__DepSet_hf5efef52__3__Slow \
	VSMT_RTL_Testbench_Core__DepSet_hf5efef52__4__Slow \
	VSMT_RTL_Testbench_Core__DepSet_hf5efef52__5__Slow \
	VSMT_RTL_Testbench_Core__DepSet_hf5efef52__6__Slow \
	VSMT_RTL_Testbench_Core__DepSet_hf5efef52__7__Slow \
	VSMT_RTL_Testbench_vm_classes_Slow_3 \
	VSMT_RTL_Testbench_vm_classes_Slow_4 \
	VSMT_RTL_Testbench_vm_classes_Slow_5 \
	VSMT_RTL_Testbench_vm_classes_Slow_6 \

# Generated support classes, fast-path, compile with highest optimization
VM_SUPPORT_FAST += \
	VSMT_RTL_Testbench__Dpi \
	VSMT_RTL_Testbench__Trace__0 \
	VSMT_RTL_Testbench__Trace__1 \
	VSMT_RTL_Testbench__Trace__2 \
	VSMT_RTL_Testbench__Trace__3 \
	VSMT_RTL_Testbench__Trace__4 \
	VSMT_RTL_Testbench__Trace__5 \
	VSMT_RTL_Testbench__Trace__6 \
	VSMT_RTL_Testbench__Trace__7 \

# Generated support classes, non-fast-path, compile with low/medium optimization
VM_SUPPORT_SLOW += \
	VSMT_RTL_Testbench__Syms \
	VSMT_RTL_Testbench__Trace__0__Slow \
	VSMT_RTL_Testbench__TraceDecls__0__Slow \
	VSMT_RTL_Testbench__Trace__1__Slow \
	VSMT_RTL_Testbench__Trace__2__Slow \
	VSMT_RTL_Testbench__Trace__3__Slow \
	VSMT_RTL_Testbench__Trace__4__Slow \
	VSMT_RTL_Testbench__Trace__5__Slow \
	VSMT_RTL_Testbench__Trace__6__Slow \
	VSMT_RTL_Testbench__Trace__7__Slow \
	VSMT_RTL_Testbench__Trace__8__Slow \
	VSMT_RTL_Testbench__Trace__9__Slow \
	VSMT_RTL_Testbench__Trace__10__Slow \

# Global classes, need linked once per executable, fast-path, compile with highest optimization
VM_GLOBAL_FAST += \
	verilated \
	verilated_dpi \
	verilated_vcd_c \
	verilated_timing \
	verilated_threads \

# Global classes, need linked once per executable, non-fast-path, compile with low/medium optimization
VM_GLOBAL_SLOW += \


# Verilated -*- Makefile -*-
