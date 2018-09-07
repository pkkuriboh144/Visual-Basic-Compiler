	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// print "Hello World"
	.section	.rodata
lab007:
	.string	"Hello World"
	.text
	movl	$lab007, %edi
	call	puts
	popq	%rbp
	ret


	.section .rodata
.output:
	.string "%d\n"

