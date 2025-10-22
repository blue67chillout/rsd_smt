# SMT Modifications Summary

This document summarizes all changes made to convert the RISC-V out-of-order processor to support Simultaneous Multi-Threading (SMT) with 2 threads.

## Overview
- **Goal**: Enable SMT by duplicating per-thread state (registers, queues) while sharing execution units with arbitration.
- **Key Changes**: Added `ThreadID` type, modified `PC_Path` to include `tid`, duplicated register files and issue queues, updated fetch arbitration, and modified branch predictors.

## 1. Basic Types and Configuration
- **File**: `BasicTypes.sv`
  - Added `THREAD_NUM = CONF_THREAD_NUM;`
  - Added `ThreadID` typedef: `logic [THREAD_NUM_BIT_WIDTH-1:0]`
- **File**: `MicroArchConf.sv`
  - Assumed `CONF_THREAD_NUM = 2;` (configurable)

## 2. PC_Path and Memory Types
- **File**: `MemoryMapTypes.sv`
  - Changed `PC_Path` from `logic [PC_WIDTH-1:0]` to `struct packed { ThreadID tid; logic [PC_WIDTH-1:0] addr; }`
  - Updated `ToAddrFromPC` and `ToPC_FromAddr` functions to handle the struct.

## 3. Register File Modifications
- **File**: `RegisterFile/RegisterFileIF.sv`
  - Added `ThreadID` signals for all read/write ports (e.g., `intSrcTidA`, `intDstTid`).
  - Updated modports to include new signals.
- **File**: `RegisterFile/RegisterFile.sv`
  - Increased RAM sizes: `.ENTRY_NUM( THREAD_NUM * PSCALAR_NUM )` for int/FP registers.
  - Modified addressing: `dstRegNum[i] = (tid * PSCALAR_NUM) + regNum.regNum`
  - Updated all read/write logic to use `tid` for indexing.

## 4. Fetch Unit Modifications
- **File**: `Pipeline/FetchStage/NextPCStageIF.sv`
  - Changed `pcOut/pcIn` to arrays `PC_Path [THREAD_NUM]`.
  - Added `ThreadID selectedTid`.
- **File**: `Pipeline/FetchStage/NextPCStage.sv`
  - Added round-robin thread selector (`threadCounter`).
  - Updated PC logic to use `pcOut[selectedTid]`, set `predNextPC.tid = selectedTid`.
  - Propagated `tid` to fetched instructions.
- **File**: `FetchUnit/FetchUnitTypes.sv`
  - Added `ThreadID tid` to `BTB_Entry`.
  - Updated `ToBTB_Index/ToBTB_Tag` to XOR `tid` into index/tag.
- **File**: `FetchUnit/BTB.sv`
  - Added `tid` check in hit logic: `btbRV[i].tid == tagReg[i].tid`
  - Set `btbWV[i].tid = port.brResult[i].brAddr.tid` on writes.
- **File**: `FetchUnit/Bimodal.sv`
  - Updated `ToPHT_Index_Local` to XOR `pc.tid` into index.
- **File**: `FetchUnit/Gshare.sv`
  - Updated `ToPHT_Index_Global` to include `pc.tid` in hash.

## 5. Issue Queue Modifications
- **File**: `Scheduler/SchedulerTypes.sv`
  - Added `ThreadID tid` to all `IssueQueueEntry` structs (`IntIssueQueueEntry`, `ComplexIssueQueueEntry`, `MemIssueQueueEntry`, `FPIssueQueueEntry`).
- **File**: `Scheduler/IssueQueue.sv` (Full Implementation Guide)
  - **Step 1: Duplicate RAMs and Free Lists Per-Thread**:
    - Change single RAMs (e.g., `intRAM`) to arrays `[THREAD_NUM]`.
    - Change `freeList` to `freeList[THREAD_NUM]`.
    - Example: `DistributedMultiPortRAM intRAM[THREAD_NUM] (...);`
  - **Step 2: Modify Enqueue Logic**:
    - Use `tid = port.writeTid[i]` to index thread's free list and RAM.
    - Set `intRAM[tid].wv[i].tid = tid;`
  - **Step 3: Modify Wakeup Logic**:
    - Add `if (t == broadcastTid)` to only wake matching thread's entries.
    - Check `intRAM[t].rv[i].tid == broadcastTid` for reg matches.
  - **Step 4: Modify Select Logic**:
    - Select per-thread: Loop over threads, find oldest ready entry per thread.
    - Arbitrate shared units: Use round-robin to choose which thread issues (e.g., `winner = roundRobinArbiter(threadHasReady);`).
  - **Step 5: Modify Flush/Recovery**:
    - Add `if (t == flushTid)` to selectively flush per-thread queues.
- **File**: `Scheduler/SchedulerIF.sv`
  - Added `ThreadID writeTid [DISPATCH_WIDTH];` to the interface for dispatch.
  - Updated `modport DispatchStage` to output `writeTid`.

## 6. CSR Unit Modifications
- **CSR_UnitIF.sv**: Added `ThreadID tid` to the interface for per-thread CSR access.
- **CSR_Unit.sv**: Added `CSR_NUM_MHARTID: rv = port.tid;` to return the thread ID on reads. Other CSRs remain shared for now.

## 7. Other Propagations
- **Interfaces**: Updated all relevant IFs (e.g., `PipelineTypes.sv`, `ControllerIF.sv`) to include `ThreadID` where needed for thread-specific operations.
- **RenameLogic**: Duplicate rename tables and free lists per-thread (modify `RenameLogic.sv`).
- **Scheduler/ActiveList**: Duplicate per-thread (modify `ActiveList.sv`).
- **LoadStoreUnit**: Duplicate load/store queues per-thread (modify LSQ modules).
- **Controller**: Add thread arbitration in pipeline control.
- **Core**: Instantiate per-thread contexts or unified multi-thread controller.

## Testbench
- **File**: `SMT_Testbench.sv`
  - Simple testbench verifying per-thread register isolation in `RegisterFile`.
  - Tests write/read for different threads, ensuring data separation.

## Notes
- **Shared Resources**: Execution units (ALU, etc.) remain shared; add `tid` arbitration in issue logic.
- **Compilation**: May require fixing interface mismatches (e.g., array sizes).
- **Testing**: Use existing SMT test code (`Verification/TestCode/C/simpsmt/`) for validation.
- **Performance**: Arbitration ensures fairness; tune round-robin or add priorities.

All changes maintain backward compatibility for single-thread mode (THREAD_NUM=1).
