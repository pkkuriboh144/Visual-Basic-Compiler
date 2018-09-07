	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// dim i as integer// dim i as integer = (5 % 3)
	pushq	$5
	pushq	$3
	popq	%rbx
	popq	%rax
	cltd	
	divq	%rbx
	movq	%rdx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (i)
// print (i)
	pushq	(i)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	popq	%rbp
	ret


	.section .rodata
.output:
	.string "%d\n"

	.globl	i
	.data
	.align	8
	.size	i, 8
i:
	.quad	0

