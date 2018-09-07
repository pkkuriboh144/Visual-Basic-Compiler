function factorial (n as integer) as integer
[
 dim ans as integer
 dim i as integer
 f = 1
 [ 
 for  i = 1 to n 
 [
	f = f*i
	printexpr(f)
 ] 
 next i 
 ]
 printstr("Complete") 
 return f
]
 end function
 [
 printexpr (factorial(10))
 ]