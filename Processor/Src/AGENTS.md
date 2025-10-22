# RSD Processor (SystemVerilog RISC-V Out-of-Order CPU)

## Build/Lint/Test Commands
- **Build simulation**: `make all` (uses Verilator)
- **Run simulation**: `make run`
- **Lint code**: `svlint --config .svlint.toml`
- **Run single test**: `make test-RV32I-ControlTransfer` (replace with specific test name, e.g., test-HelloWorld, test-Coremark)
- **Run all tests**: `make test-all` (includes test-1 and test-2 levels)
- **Build test code**: `make test-build`

## Architecture and Codebase Structure
- **Core modules**: Controller.sv, Core.sv, Pipeline/, ExecUnit/, Scheduler/, RegisterFile/, RenameLogic/
- **Subprojects**: Verification/ (test code), Makefiles/ (build scripts), Decoder/, FetchUnit/, LoadStoreUnit/, MulDivUnit/, FloatingPointUnit/
- **Configuration**: MicroArchConf.sv (tunable parameters like FETCH_WIDTH=2, PSCALAR_NUM=64)
- **Interfaces**: ControllerIF.sv (inter-module communication)
- **Types**: BasicTypes.sv (typedefs for DataPath, AddrPath, OpSrc, etc.)
- **Tools**: ../Tools/ (KanataConverter, TestDriver)

## Code Style Guidelines
- **Language**: SystemVerilog 2012, packages for types/constants
- **Naming**: CamelCase for types (e.g., AddrPath), UPPER_CASE for constants, snake_case for signals
- **Formatting**: 4-space indentation, align struct fields, use typedef for complex types
- **Imports**: `import MicroArchConf::*;` at top of packages
- **Error handling**: Assertions enabled in Verilator, check disabled warnings in Makefile
- **Macros**: Use `ifdef RSD_MARCH_FP_PIPE` for FP support, avoid legacy always blocks per lint rules
