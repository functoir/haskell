.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c1uB_str:
	.asciz "Hello World!"
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.Ls1uu_info)+0
.Ls1uu_info:
.Lc1uC:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1uD
.Lc1uE:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc1uA
.Lc1uz:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $c1uB_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc1uA:
	jmp *(%rbx)
.Lc1uD:
	jmp *-16(%r13)
	.size .Ls1uu_info, .-.Ls1uu_info
.section .data
.align 8
.align 1
.Ls1uu_closure:
	.quad	.Ls1uu_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.Lu1uV_srt:
	.quad	stg_SRT_2_info
	.quad	base_SystemziIO_putStrLn_closure
	.quad	.Ls1uu_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.Lu1uV_srt-(Main_main_info)+0
.globl Main_main_info
.type Main_main_info, @function
Main_main_info:
.Lc1uS:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1uT
.Lc1uU:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc1uR
.Lc1uQ:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $.Ls1uu_closure,%r14d
	movl $base_SystemziIO_putStrLn_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_p_fast
.Lc1uR:
	jmp *(%rbx)
.Lc1uT:
	jmp *-16(%r13)
	.size Main_main_info, .-Main_main_info
.section .data
.align 8
.align 1
.globl Main_main_closure
.type Main_main_closure, @object
Main_main_closure:
	.quad	Main_main_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.Lu1vb_srt:
	.quad	stg_SRT_2_info
	.quad	base_GHCziTopHandler_runMainIO_closure
	.quad	Main_main_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.Lu1vb_srt-(ZCMain_main_info)+0
.globl ZCMain_main_info
.type ZCMain_main_info, @function
ZCMain_main_info:
.Lc1v8:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1v9
.Lc1va:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc1v7
.Lc1v6:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $Main_main_closure,%r14d
	movl $base_GHCziTopHandler_runMainIO_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_p_fast
.Lc1v7:
	jmp *(%rbx)
.Lc1v9:
	jmp *-16(%r13)
	.size ZCMain_main_info, .-ZCMain_main_info
.section .data
.align 8
.align 1
.globl ZCMain_main_closure
.type ZCMain_main_closure, @object
ZCMain_main_closure:
	.quad	ZCMain_main_info
	.quad	0
	.quad	0
	.quad	0
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.Lr1hA_bytes:
	.asciz "main"
.section .data
.align 8
.align 1
.Lr1hS_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	.Lr1hA_bytes
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.Lr1hT_bytes:
	.asciz "Main"
.section .data
.align 8
.align 1
.Lr1hU_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	.Lr1hT_bytes
.section .data
.align 8
.align 1
.globl Main_zdtrModule_closure
.type Main_zdtrModule_closure, @object
Main_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	.Lr1hS_closure+1
	.quad	.Lr1hU_closure+1
	.quad	3
.section .note.GNU-stack,"",@progbits
.ident "GHC 8.8.4"


