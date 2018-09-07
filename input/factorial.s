	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// print ((factorial (10)))
	pushq	$10
	call	factorial
	addq	$8, %rsp
	pushq	%rax
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	popq	%rbp
	ret

	.text
	.globl	factorial
	.type	factorial, @function
factorial:
	pushq	%rbp
	movq	%rsp, %rbp
// dim ans as integer// dim i as integer// dim f as integer = 1
	pushq	$1
	popq	%rax
	movq	%rax, (f)
// for i = 1 to n
	pushq	$1
	popq	%rax
	movq	%rax, (i)
	jmp	lab005
lab004:
// dim f as integer = (f * i)
	pushq	(f)
	pushq	(i)
	popq	%rbx
	popq	%rax
	imulq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (f)
// print (f)
	pushq	(f)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	movq	(i), %rax
	addq	$1, %rax
	movq	%rax, (i)
lab005:
	pushq	16(%rbp)
	popq	%rbx
	movq	(i), %rax
	cmpq	%rbx, %rax
	jle	lab004
// print "Complete"
	.section	.rodata
lab006:
	.string	"Complete"
	.text
	movl	$lab006, %edi
	call	puts
// return (f)
	pushq	(f)
	popq	%rax
	popq	%rbp
	ret
	popq	%rbp
	ret

	.section .rodata
.output:
	.string "%d\n"

	.globl	ans
	.data
	.align	8
	.size	ans, 8
ans:
	.quad	0

	.globl	f
	.data
	.align	8
	.size	f, 8
f:
	.quad	0

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

