// A simple multi-threaded program for trace analysis on an SMT core.

// Shared global variables for results - volatile to prevent optimization
// and ensure visibility between threads.
volatile int result_thread0 = 0;
volatile int result_thread1 = 0;

// --- Thread 0: Simple Counter ---
// This thread increments a local variable a few times.
void main_thread0() {
    int counter = 0;
    for (int i = 0; i < 5; i++) {
        counter = counter + 1;
    }
    // Store the final count (5) into the shared variable.
    result_thread0 = counter; 
}

// --- Thread 1: Simple Arithmetic Sequence ---
// This thread calculates a value using a small loop.
void main_thread1() {
    int value = 10;
    for (int i = 0; i < 3; i++) {
        value = (value * 2) - i; // Sequence: (10*2-0)=20, (20*2-1)=39, (39*2-2)=76
    }
    // Store the final value (76) into the shared variable.
    result_thread1 = value;
}
