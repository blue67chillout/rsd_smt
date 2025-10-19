#include "Asm/rsd-asm-macros.h"

    .file    "rsd-crt.s"
    .text
    .align    4

    .global _start
    .global _end # Keep the global symbol declaration

    .extern _load
    .extern main_thread0
    .extern main_thread1
    .extern __stack_top_thread0
    .extern __stack_top_thread1

_start:
    j _init              # Jump to initialization code
                         # REMOVED _end label from here

_init:
    # Set trap vector
    la a0, trap_vector
    csrw mtvec, a0

    # Read hartid
    csrr t0, mhartid

    # Load stack pointer based on hartid
    lui   sp, %hi(__stack_top_thread0)
    addi  sp, sp, %lo(__stack_top_thread0)
    bnez  t0, setup_thread1

setup_thread0:
    j     call_loader

setup_thread1:
    lui   sp, %hi(__stack_top_thread1)
    addi  sp, sp, %lo(__stack_top_thread1)

call_loader:
    # Call C loader
    mv    a0, t0
    call  _load

    # --- Clear registers ---
    # (Register clearing code remains here)
    li    x1, 0
    # ... etc ...
    li    x31, 0

    # --- Call correct main ---
    csrr t0, mhartid
    bnez t0, call_main1

call_main0:
    call  main_thread0
    j     _halt_loop      # Jump to the relocated halt loop

call_main1:
    call  main_thread1
    # Fall through to the halt loop

# --- NEW LOCATION FOR THE HALT LOOP ---
_halt_loop:             # Renamed for clarity, but could still be _end
_end:                   # Define the _end symbol here
    j _end               # Infinite loop located within .text

# --- Trap Vector ---
trap_vector:
    # (Trap handler code remains the same)
    # ...
abort:
    j _halt_loop         # Ensure exceptions also go to the halt loop
    # ... (rest of trap handler) ...

    .data
    # (Data section remains the same)
