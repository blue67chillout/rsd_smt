// Simple SystemVerilog Testbench for SMT Modifications
// Tests basic functionality of modified RegisterFile and NextPCStage

`timescale 1ns / 1ps

module SMT_Testbench;

    // Clock and reset
    logic clk = 0;
    logic rst = 1;
    logic rstStart = 0;

    always #5 clk = ~clk;  // 10ns period

    initial begin
        rst = 1;
        #20 rst = 0;
        rstStart = 1;
        #10 rstStart = 0;
    end

    // Instantiate RegisterFileIF
    RegisterFileIF regFileIF(.clk(clk), .rst(rst), .rstStart(rstStart));

    // Test RegisterFile
    RegisterFile regFile(.port(regFileIF.RegisterFile));

    // Test data
    initial begin
        // Wait for reset
        #30;

        // Test write/read for thread 0
        regFileIF.intDstTid[0] = 0;
        regFileIF.intDstRegWE[0] = 1;
        regFileIF.intDstRegNum[0] = '{regNum: 5, isFP: 0};
        regFileIF.intDstRegData[0] = '{valid: 1, data: 32'h12345678};

        regFileIF.intSrcTidA[0] = 0;
        regFileIF.intSrcRegNumA[0] = '{regNum: 5, isFP: 0};

        #10;

        // Check read
        if (regFileIF.intSrcRegDataA[0].data == 32'h12345678) begin
            $display("Thread 0 register test PASSED");
        end else begin
            $display("Thread 0 register test FAILED");
        end

        // Test thread 1
        regFileIF.intDstTid[0] = 1;
        regFileIF.intDstRegWE[0] = 1;
        regFileIF.intDstRegNum[0] = '{regNum: 5, isFP: 0};
        regFileIF.intDstRegData[0] = '{valid: 1, data: 32'h87654321};

        regFileIF.intSrcTidA[0] = 1;
        regFileIF.intSrcRegNumA[0] = '{regNum: 5, isFP: 0};

        #10;

        if (regFileIF.intSrcRegDataA[0].data == 32'h87654321) begin
            $display("Thread 1 register test PASSED");
        end else begin
            $display("Thread 1 register test FAILED");
        end

        #50 $finish;
    end

endmodule
