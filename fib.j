.class public Fib.Fib
.super java/lang/Object

.method public static write(I)V
    .limit locals 1
    .limit stack 2
    getstatic java/lang/System/out Ljava/io/PrintStream;
    iload 0
    invokevirtual java/io/PrintStream/print(I)V
    return
.end method

.method public static writes(Ljava/lang/String;)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 0
    invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
    return
.end method

.method public static read()I
    .limit locals 10
    .limit stack 10

    ldc 0
    istore 1    ; this will hold our final integer
Label1:
    getstatic java/lang/System/in Ljava/io/InputStream;
    invokevirtual java/io/InputStream/read()I
    istore 2
    iload 2
    ldc 10    ; test for the newline delimiter for Unix
    isub
    ifeq Label2
    iload 2
    ldc 13    ; test for the carriage-return in Windows
    isub
    ifeq Label2
    iload 2
    ldc 32    ; the space delimiter
    isub
    ifeq Label2
    iload 2
    ldc 48 ; we have our digit in AsCIl, have to subtract it from 48
    isub
    ldc 10
    iload 1
    imul
    iadd
    istore 1
    goto Label1
Label2:
    ; when we come here we have our integer computed
    ; in local variable 1
    iload 1
    ireturn
.end method

.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

; COMPILED CODE STARTS

   ldc "Fib: "
   invokestatic Fib/Fib/writes(Ljava/lang/String;)V
   invokestatic Fib/Fib/read()I
   istore 0 		; n
   ldc 1
   istore 1 		; minus1
   ldc 0
   istore 2 		; minus2
Loop_begin_1:
   iload 0 		; n
   ldc 0
   if_icmple Loop_end_2
   iload 2 		; minus2
   istore 3 		; temp
   iload 1 		; minus1
   iload 2 		; minus2
   iadd
   istore 2 		; minus2
   iload 3 		; temp
   istore 1 		; minus1
   iload 0 		; n
   ldc 1
   isub
   istore 0 		; n
   goto Loop_begin_1
Loop_end_2:
   ldc "Result: "
   invokestatic Fib/Fib/writes(Ljava/lang/String;)V
   iload 2 		; minus2
   invokestatic Fib/Fib/write(I)V
   ldc "\n"
   invokestatic Fib/Fib/writes(Ljava/lang/String;)V
exit_0:

; COMPILED CODE ENDS
   return

.end method
