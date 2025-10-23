// Copyright 2019- RSD contributors.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.


//
// PC
// PC has INSN_RESET_VECTOR and cannot use AddrReg.
//

import BasicTypes::*;
import MemoryMapTypes::*;

module PC( NextPCStageIF.PC port );

    // Per-thread PC FFs
    PC_Path pcRegs[THREAD_NUM];

    generate
    for (genvar t = 0; t < THREAD_NUM; t++) begin
        always_ff @(posedge port.clk) begin
            if (port.rst) begin
                pcRegs[t] <= '{tid: t, addr: INSN_RESET_VECTOR};
            end else if (port.pcWE && (t == port.selectedTid)) begin
                pcRegs[t] <= port.pcIn[t];
            end
        end
    end
    endgenerate

    assign port.pcOut = pcRegs[port.selectedTid];

endmodule : PC

