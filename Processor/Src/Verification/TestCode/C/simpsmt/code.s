	.file	"code.c"
	.option nopic
	.attribute arch, "rv32i2p1_m2p0_f2p2_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
.Ltext0:
	.cfi_sections	.debug_frame
	.file 0 "/home/friday/rsd_smt/Processor/Src/Verification/TestCode/C/simpsmt" "code.c"
	.align	2
	.globl	main_thread0
	.type	main_thread0, @function
main_thread0:
.LFB0:
	.file 1 "code.c"
	.loc 1 10 21
	.cfi_startproc
	.loc 1 11 5
.LVL0:
	.loc 1 12 5
	.loc 1 12 10
	.loc 1 12 23 discriminator 1
	.loc 1 16 5
	.loc 1 16 20 is_stmt 0
	lui	a5,%hi(result_thread0)
	li	a4,5
	sw	a4,%lo(result_thread0)(a5)
	.loc 1 17 1
	ret
	.cfi_endproc
.LFE0:
	.size	main_thread0, .-main_thread0
	.align	2
	.globl	main_thread1
	.type	main_thread1, @function
main_thread1:
.LFB1:
	.loc 1 21 21 is_stmt 1
	.cfi_startproc
	.loc 1 22 5
.LVL1:
	.loc 1 23 5
	.loc 1 23 10
	.loc 1 23 23 discriminator 1
	.loc 1 24 9
	.loc 1 23 29 discriminator 3
	.loc 1 23 23 discriminator 1
	.loc 1 24 9
	.loc 1 23 29 discriminator 3
	.loc 1 23 23 discriminator 1
	.loc 1 24 9
	.loc 1 23 29 discriminator 3
	.loc 1 23 23 discriminator 1
	.loc 1 27 5
	.loc 1 27 20 is_stmt 0
	lui	a5,%hi(result_thread1)
	li	a4,76
	sw	a4,%lo(result_thread1)(a5)
	.loc 1 28 1
	ret
	.cfi_endproc
.LFE1:
	.size	main_thread1, .-main_thread1
	.globl	result_thread1
	.globl	result_thread0
	.section	.sdata,"aw"
	.align	2
	.type	result_thread1, @object
	.size	result_thread1, 4
result_thread1:
	.zero	4
	.type	result_thread0, @object
	.size	result_thread0, 4
result_thread0:
	.zero	4
	.text
.Letext0:
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.4byte	0xa6
	.2byte	0x5
	.byte	0x1
	.byte	0x4
	.4byte	.Ldebug_abbrev0
	.uleb128 0x4
	.4byte	.LASF5
	.byte	0x1d
	.4byte	.LASF0
	.4byte	.LASF1
	.4byte	.Ltext0
	.4byte	.Letext0-.Ltext0
	.4byte	.Ldebug_line0
	.uleb128 0x1
	.4byte	.LASF2
	.byte	0x5
	.4byte	0x3d
	.uleb128 0x5
	.byte	0x3
	.4byte	result_thread0
	.uleb128 0x5
	.byte	0x4
	.byte	0x5
	.string	"int"
	.uleb128 0x6
	.4byte	0x36
	.uleb128 0x1
	.4byte	.LASF3
	.byte	0x6
	.4byte	0x3d
	.uleb128 0x5
	.byte	0x3
	.4byte	result_thread1
	.uleb128 0x7
	.4byte	.LASF6
	.byte	0x1
	.byte	0x15
	.byte	0x6
	.4byte	.LFB1
	.4byte	.LFE1-.LFB1
	.uleb128 0x1
	.byte	0x9c
	.4byte	0x80
	.uleb128 0x8
	.4byte	.LASF7
	.byte	0x1
	.byte	0x16
	.byte	0x9
	.4byte	0x36
	.byte	0x4c
	.uleb128 0x2
	.uleb128 0x3
	.string	"i"
	.byte	0x17
	.4byte	0x36
	.byte	0
	.byte	0
	.uleb128 0x9
	.4byte	.LASF8
	.byte	0x1
	.byte	0xa
	.byte	0x6
	.4byte	.LFB0
	.4byte	.LFE0-.LFB0
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0xa
	.4byte	.LASF4
	.byte	0x1
	.byte	0xb
	.byte	0x9
	.4byte	0x36
	.uleb128 0x2
	.uleb128 0x3
	.string	"i"
	.byte	0xc
	.4byte	0x36
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.uleb128 0x1
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0x21
	.sleb128 1
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0x21
	.sleb128 14
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x2
	.uleb128 0xb
	.byte	0x1
	.byte	0
	.byte	0
	.uleb128 0x3
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0x21
	.sleb128 1
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0x21
	.sleb128 14
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x4
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0xe
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x1f
	.uleb128 0x1b
	.uleb128 0x1f
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.uleb128 0x10
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x5
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.byte	0
	.byte	0
	.uleb128 0x6
	.uleb128 0x35
	.byte	0
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x7
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x7a
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x8
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x1c
	.uleb128 0xb
	.byte	0
	.byte	0
	.uleb128 0x9
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x6
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x7a
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0xa
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x39
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_aranges,"",@progbits
	.4byte	0x1c
	.2byte	0x2
	.4byte	.Ldebug_info0
	.byte	0x4
	.byte	0
	.2byte	0
	.2byte	0
	.4byte	.Ltext0
	.4byte	.Letext0-.Ltext0
	.4byte	0
	.4byte	0
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.section	.debug_str,"MS",@progbits,1
.LASF7:
	.string	"value"
.LASF4:
	.string	"counter"
.LASF2:
	.string	"result_thread0"
.LASF3:
	.string	"result_thread1"
.LASF8:
	.string	"main_thread0"
.LASF6:
	.string	"main_thread1"
.LASF5:
	.string	"GNU C17 14.2.0 -mstrict-align -mabi=ilp32 -misa-spec=20191213 -march=rv32imf_zicsr -g -g -O3 -fno-stack-protector -fno-zero-initialized-in-bss -ffreestanding -fno-builtin"
	.section	.debug_line_str,"MS",@progbits,1
.LASF0:
	.string	"code.c"
.LASF1:
	.string	"/home/friday/rsd_smt/Processor/Src/Verification/TestCode/C/simpsmt"
	.ident	"GCC: (xPack GNU RISC-V Embedded GCC x86_64) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
