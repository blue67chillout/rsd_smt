
// A better C program that writes distinct, calculated values to multiple registers
// using inline assembly.

int main() {
    // --- Perform distinct calculations ---

    // 1. Arithmetic test
    int arithmetic_res = (10 + 20) * 3; // Expected: 90 (0x5A)

    // 2. Bitwise logic test
    // (0xFF00A5A5 & 0x00FFFF00) results in 0x0000A500
    // 0x0000A500 ^ 0x12345678 results in 0x1234F378
    int bitwise_res = (0xFF00A5A5 & 0x00FFFF00) ^ 0x12345678;

    // 3. Shift test (logical right shift)
    unsigned int shift_res = 0x80000001 >> 5; // Expected: 0x04000000


    // --- Use inline assembly to move results to specific registers ---
    // We will target registers t0 (x5), t1 (x6), and t2 (x7).
    // The "volatile" keyword prevents the compiler from removing these instructions.
    asm volatile ("mv t0, %0" :: "r"(arithmetic_res));
    asm volatile ("mv t1, %0" :: "r"(bitwise_res));
    asm volatile ("mv t2, %0" :: "r"(shift_res));


    // --- Return a final value for a0 ---
    // For verification, return a checksum of the results.
    return arithmetic_res + bitwise_res + shift_res;
}
