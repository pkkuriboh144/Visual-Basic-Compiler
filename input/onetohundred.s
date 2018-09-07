	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// return ((onetohun (100)))
	pushq	$100
	call	onetohun
	addq	$8, %rsp
	pushq	%rax
	popq	%rax
	popq	%rbp
	ret
	popq	%rbp
	ret

	.text
	.globl	onetohun
	.type	onetohun, @function
onetohun:
	pushq	%rbp
	movq	%rsp, %rbp
// dim i as integer// dim n as integer = n
	pushq	16(%rbp)
	popq	%rax
	movq	%rax, 16(%rbp)
// for i = 1 to n
	pushq	$1
	popq	%rax
	movq	%rax, (i)
	jmp	lab002
lab001:
// print (i)
	pushq	(i)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	movq	(i), %rax
	addq	$1, %rax
	movq	%rax, (i)
lab002:
	pushq	16(%rbp)
	popq	%rbx
	movq	(i), %rax
	cmpq	%rbx, %rax
	jle	lab001
// print "Complete"
	.section	.rodata
lab003:
	.string	"Complete"
	.text
	movl	$lab003, %edi
	call	puts
// return (0)
	pushq	$0
	popq	%rax
	popq	%rbp
	ret
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

	.globl	n
	.data
	.align	8
	.size	n, 8
n:
	.quad	0

