# Makefile for SMT RTL Testbench
# Based on the main Makefile, adapted for SMT_RTL_Testbench.sv

MAX_TEST_CYCLES = 100000
SHOW_SERIAL_OUT = 1
ENABLE_PC_GOAL = 1

ifndef RSD_VERILATOR_BIN
VERILATOR_BIN = verilator
else
VERILATOR_BIN = $(RSD_VERILATOR_BIN)
endif

SOURCE_ROOT  = ./
TOOLS_ROOT   = ../Tools/
PROJECT_WORK =  ../Project/Verilator_SMT
LIBRARY_WORK_RTL = $(PROJECT_WORK)/obj_dir

TOP_MODULE = SMT_RTL_Testbench
VERILATED_TOP_MODULE_NAME = V$(TOP_MODULE)

# Include core source code definition
include Makefiles/CoreSources.inc.mk

# Add testbench to dependencies
DEPS_RTL = \
	$(TYPES:%=$(SOURCE_ROOT)%) \
	$(MODULES:%=$(SOURCE_ROOT)%) \
	$(TEST_MODULES:%=$(SOURCE_ROOT)%) \
	$(SOURCE_ROOT)SMT_RTL_Testbench.sv

# Temporally disabled warnings
VERILATOR_DISABLED_WARNING = \
     -Wno-WIDTH \
     -Wno-INITIALDLY \
     -Wno-UNOPTFLAT \
	 -Wno-TIMESCALEMOD \
	 -Wno-LATCH \

# RSD specific constants
RSD_VERILATOR_DEFINITION = \
	+define+RSD_FUNCTIONAL_SIMULATION \
	+define+RSD_FUNCTIONAL_SIMULATION_VERILATOR \
	$(RSD_SRC_CFG) \

# --assert: Enable all assertions.
# --Mdir: Name of output object directory.
# We use \"-Os\" and \"-output-split 15000\" for faster compilation.
# See https://www.veripool.org/papers/Verilator_Accelerated_OSDA2020.pdf
VERILATOR_OPTION = \
	--cc \
	--binary \
	--assert \
	-sv \
	--top-module $(TOP_MODULE) \
	$(VERILATOR_DISABLED_WARNING) \
	$(RSD_VERILATOR_DEFINITION) \
	--Mdir $(LIBRARY_WORK_RTL) \
	+incdir+. \
	--trace \
	--trace-structs \
	-output-split 15000 \
	-j 0 \
	#-CFLAGS \"-Os -include limits\" \
	#-CFLAGS \"-O0 -g\" \
	#--MMD \
	#-O3 \

VERILATOR_TARGET_CXXFLAGS= \
	-D RSD_FUNCTIONAL_SIMULATION_VERILATOR \
	-D RSD_FUNCTIONAL_SIMULATION \
	-D RSD_VERILATOR_TRACE \
	-D RSD_MARCH_FP_PIPE \
	-Wno-attributes \

all: $(LIBRARY_WORK_RTL) $(DEPS_RTL) Makefiles/CoreSources.inc.mk
	$(VERILATOR_BIN) $(VERILATOR_OPTION) $(DEPS_RTL)
	cd $(LIBRARY_WORK_RTL); \
		VPATH=../../../Src \
		CXXFLAGS="$(VERILATOR_TARGET_CXXFLAGS)" \
			$(MAKE) -f $(VERILATED_TOP_MODULE_NAME).mk
	@echo "==== Build Successful ===="

run:
	$(LIBRARY_WORK_RTL)/$(VERILATED_TOP_MODULE_NAME) \
		+MAX_TEST_CYCLES=$(MAX_TEST_CYCLES) \
		+TEST_CODE=$(TEST_CODE) +ENABLE_PC_GOAL=$(ENABLE_PC_GOAL) +SHOW_SERIAL_OUT=$(SHOW_SERIAL_OUT)

$(LIBRARY_WORK_RTL):
	mkdir -p $(LIBRARY_WORK_RTL)

clean:
	rm -rf $(LIBRARY_WORK_RTL)

.PHONY: all run clean
