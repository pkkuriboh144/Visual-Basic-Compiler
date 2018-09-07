function onetohun (n as integer) as integer
[
 dim i as integer
 n = n
 [ for  i = 1 to n 
 [printexpr(i)] 
 next i ]
 printstr("Complete") 
 return 0
 ] 
 end function
 [return onetohun(100)]