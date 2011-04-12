.source Example.j
.class public Example
.super java/lang/Object

.method public static main([Ljava/lang/String;)V
  .limit locals 1
  .limit stack 2
  
  ldc2_w 3.14
  invokestatic Runtime/printDouble(D)V
  
  return
.end method