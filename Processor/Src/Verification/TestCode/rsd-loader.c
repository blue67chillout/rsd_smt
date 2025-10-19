#include <stdint.h>
#include <stddef.h>

// --- Linker script symbols ---
// These provide the start/end addresses for memory sections.
extern int __rodata_end[]; // End of read-only data (where .data initial values start in ROM)
extern int __data_start[]; // Start address of .data section in RAM
extern int __data_end[];   // End address of .data section in RAM
extern int __bss_start[];  // Start address of .bss section in RAM
extern int __bss_end[];    // End address of .bss section in RAM

// --- Synchronization flag for multi-threading ---
// This flag signals when the shared memory (.data, .bss) has been initialized.
// 'volatile' is CRUCIAL to prevent the compiler from optimizing away the checks
// in the spin-wait loop and to ensure visibility between threads.
volatile uint32_t __memory_setup_done = 0; // Initialize to 0

// Simple implementation of memcpy (byte-by-byte copy)
static void* _rsd_memcpy(void* dest_, const void* src_, size_t n) {
    uint8_t* dest = (uint8_t*)dest_;
    const uint8_t* src = (const uint8_t*)src_;
    for (size_t i = 0; i < n; i++) {
        dest[i] = src[i];
    }
    return dest;
}

// Simple implementation of memset (byte-by-byte set)
static void* _rsd_memset(void* str_, int c, size_t n) {
    uint8_t* str = (uint8_t*)str_;
    for (size_t i = 0; i < n; i++) {
        str[i] = (uint8_t)c;
    }
    return str;
}

// --- Modified _load function for SMT ---
// This function is called by the assembly startup code (rsd-crt.s)
// for EACH hardware thread. It now takes the hartid as an argument.
void _load(uintptr_t hartid) { // uintptr_t is suitable for hart IDs

    // --- ONLY THREAD 0 performs memory initialization ---
    if (hartid == 0) {
        // 1. Calculate the size of the .data section.
        size_t data_size = (size_t)((uintptr_t)&__data_end - (uintptr_t)&__data_start);

        // 2. Copy initialized data from the end of ROM (__rodata_end) to RAM (__data_start).
        // Note: The original code used ram_start which might be incorrect if .data doesn't start exactly at ram_start.
        // Using __data_start as the destination is generally safer.
        _rsd_memcpy(&__data_start, &__rodata_end, data_size);

        // 3. Calculate the size of the .bss section.
        size_t bss_size = (size_t)((uintptr_t)&__bss_end - (uintptr_t)&__bss_start);

        // 4. Clear the BSS section (set all bytes to 0).
        _rsd_memset(&__bss_start, 0, bss_size);

        // 5. Signal to other threads that shared memory setup is complete.
        // Ensure memory operations are complete before setting the flag (memory barrier might be needed on complex cores).
        __memory_setup_done = 1;

    } else {
        // --- Other threads (hartid != 0) WAIT for setup to complete ---
        while (__memory_setup_done == 0) {
            // Spin-wait loop: Keep checking the flag until Thread 0 sets it.
            // On a real system, you might add a short delay or yield instruction here
            // to avoid consuming excessive power/resources in the spin loop.
            asm volatile ("nop"); // Simple way to prevent an empty loop potentially being optimized away
        }
    }
    // All threads proceed from here only AFTER memory is initialized and the flag is set.
}
