{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Run-Time type information routines - processor dependent part }

Procedure Initialize (Data,TypeInfo : pointer);[Public,Alias:'FPC_INITIALIZE'];assembler;
asm
// Save registers
        push    %eax
        push    %ebx
        push    %ecx
        push    %edx
// decide what type it is
        movl    12(%ebp),%ebx
        movb    (%ebx),%al
// This is MANIFESTLY wrong
        subb    $9,%al
        jz      .LDoAnsiStringInit
        decb    %al
        jz      .LDoAnsiStringInit
        subb    $3,%al
        jz      .LDoArrayInit
        decb    %al
        jz      .LDoRecordInit
        decb    %al
        jz      .LDoClassInit
        decb    %al
        jz      .LDoObjectInit
        decb    %al
        jz      .LDoClassInit
        jmp     .LExitInitialize
.LDoObjectInit:
.LDoClassInit:
.LDoRecordInit:
        incl    %ebx
        movzbl  (%ebx),%eax
// Skip also recordsize.
        addl    $5,%eax
        addl    %eax,%ebx
// %ebx points to element count. Set in %edx
        movl    (%ebx),%edx
        addl    $4,%ebx
// %ebx points to First element in record
.LMyRecordInitLoop:
        decl    %edx
        jl      .LExitInitialize
// %ebx points to typeinfo pointer
// Push type
        pushl    (%ebx)
        addl     $4,%ebx
// %ebx points to offset in record.
// Us it to calculate data
        movl    8(%ebp),%eax
        addl    (%ebx),%eax
        addl     $4,%ebx
// push data
        pushl    %eax
        call    INITIALIZE
        jmp     .LMyRecordInitLoop
// Array handling
.LDoArrayInit:
// Skip array name !!
        incl    %ebx
        movzbl  (%ebx),%eax
        incl    %eax
        addl    %eax,%ebx
// %ebx points to size. Put size in ecx
        movl    (%ebx),%ecx
        addl    $4, %ebx
// %ebx points to count. Put count in %edx
        movl    (%ebx),%edx
        addl    $4, %ebx
// %ebx points to type. Put into ebx.
// Start treating elements.
.LMyArrayInitLoop:
        decl    %edx
        jl      .LExitInitialize
// push type
        pushl   (%ebx)
// calculate data
        movl    %ecx,%eax
        imull    %edx,%eax
        addl    8(%ebp),%eax
// push data
        pushl   %eax
        call    INITIALIZE
        jmp     .LMyArrayInitLoop
// AnsiString handling :
.LDoAnsiStringInit:
        movl 8(%ebp), %eax
        movl $0,(%eax)
.LExitInitialize:
        pop     %edx
        pop     %ecx
        pop     %ebx
        pop     %eax
end;


Procedure Finalize (Data,TypeInfo: Pointer);[Public,Alias:'FPC_FINALIZE'];assembler;
asm
        push    %eax
        push    %ebx
        push    %ecx
        push    %edx
// decide what type it is
        movl    12(%ebp),%ebx
        movb    (%ebx),%al
        subb    $9,%al
        jz      .LDoAnsiStringFinal
        decb    %al
        jz      .LDoAnsiStringFinal
        subb    $3,%al
        jz      .LDoArrayFinal
        decb    %al
        jz      .LDoRecordFinal
        decb    %al
        jz      .LDoClassFinal
        decb    %al
        jz      .LDoObjectFinal
        decb    %al
        jz      .LDoClassFinal
        jmp     .LExitFinalize
.LDoClassFinal:
.LDoObjectFinal:
.LDoRecordFinal:
        incl    %ebx
        movzbl  (%ebx),%eax
// Skip also recordsize.
        addl    $5,%eax
        addl    %eax,%ebx
// %ebx points to element count. Set in %edx
        movl    (%ebx),%edx
        addl    $4,%ebx
// %ebx points to First element in record
.LMyRecordFinalLoop:
        decl    %edx
        jl      .LExitFinalize
// %ebx points to typeinfo pointer
// Push type
        pushl    (%ebx)
        addl     $4,%ebx
// %ebx points to offset.
// Use to calculate data
        movl    8(%ebp),%eax
        addl    (%ebx),%eax
        addl     $4,%ebx
// push data
        pushl    %eax
        call    FINALIZE
        jmp     .LMyRecordFinalLoop
// Array handling
.LDoArrayFinal:
// Skip array name !!
        incl    %ebx
        movzbl  (%ebx),%eax
        incl    %eax
        addl    %eax,%ebx
// %ebx points to size. Put size in ecx
        movl    (%ebx),%ecx
        addl    $4, %ebx
// %ebx points to count. Put count in %edx
        movl    (%ebx),%edx
        addl    $4, %ebx
// %ebx points to type. Put into ebx.
// Start treating elements.
.LMyArrayFinalLoop:
        decl    %edx
        jl      .LExitFinalize
// push type
        pushl   (%ebx)
// calculate data
        movl    %ecx,%eax
        imull    %edx,%eax
        addl    8(%ebp),%eax
// push data
        pushl   %eax
        call    FINALIZE
        jmp     .LMyArrayFinalLoop
// AnsiString handling :
.LDoAnsiStringFinal:
        pushl   8(%ebp)
        call    ANSISTR_DECR_REF
.LExitFinalize:
        pop     %edx
        pop     %ecx
        pop     %ebx
        pop     %eax
end;


Procedure Addref (Data,TypeInfo : Pointer); [Public,alias : 'FPC_ADDREF'];Assembler;
asm
// Save registers
        push    %eax
        push    %ebx
        push    %ecx
        push    %edx
// decide what type it is
        movl    12(%ebp),%ebx
        movb    (%ebx),%al
        subb    $9,%al
        jz      .LDoAnsiStringAddRef
        decb    %al
        jz      .LDoAnsiStringAddRef
        subb    $2,%al
        jz      .LDoArrayAddRef
        decb    %al
        jz      .LDoRecordAddRef
        decb    %al
        jz      .LDoClassAddRef
        decb    %al
        jz      .LDoObjectAddRef
        decb    %al
        jz      .LDoClassAddRef
        jmp     .LExitAddRef
.LDoClassAddRef:
.LDoObjectAddRef:
.LDoRecordAddRef:
        incl    %ebx
        movzbl  (%ebx),%eax
// Skip also recordsize.
        addl    $5,%eax
        addl    %eax,%ebx
// %ebx points to element count. Set in %edx
        movl    (%ebx),%edx
        addl    $4,%ebx
// %ebx points to First element in record
.LMyRecordAddRefLoop:
        decl    %edx
        jl      .LExitAddRef
// Calculate data
        movl    8(%ebp),%eax
        addl    (%ebx),%eax
        addl     $4,%ebx
// Push type
        pushl    (%ebx)
        addl     $4,%ebx
// push data
        pushl    %eax
        call    ADDREF
        jmp     .LMyRecordAddRefLoop
// Array handling
.LDoArrayAddRef:
// %ebx points to size. Put size in ecx
        movl    (%ebx),%ecx
        addl    $4, %ebx
// %ebx points to count. Put count in %edx
        movl    (%ebx),%edx
        addl    $4, %ebx
// %ebx points to type. Put into ebx.
// Start treating elements.
.LMyArrayAddRefLoop:
        decl    %edx
        jl      .LExitAddRef
// push type
        pushl   (%ebx)
// calculate data
        movl    %ecx,%eax
        imull    %edx,%eax
        addl    8(%ebp),%eax
// push data
        pushl   %eax
        call    ADDREF
        jmp     .LMyArrayAddRefLoop
// AnsiString handling :
.LDoAnsiStringAddRef:
        pushl   8(%ebp)
        call    ANSISTR_INCR_REF
.LExitAddRef:
        pop     %edx
        pop     %ecx
        pop     %ebx
        pop     %eax
end;


Procedure DecRef (Data,TypeInfo : Pointer); [Public,alias : 'FPC_DECREF'];Assembler;
asm
// Save registers
        push    %eax
        push    %ebx
        push    %ecx
        push    %edx
// decide what type it is
        movl    12(%ebp),%ebx
        movb    (%ebx),%al
        subb    $9,%al
        jz      .LDoAnsiStringDecRef
        decb    %al
        jz      .LDoAnsiStringDecRef
        subb    $2,%al
        jz      .LDoArrayDecRef
        decb    %al
        jz      .LDoRecordDecRef
        decb    %al
        jz      .LDoClassDecRef
        decb    %al
        jz      .LDoObjectDecRef
        decb    %al
        jz      .LDoClassDecRef
        jmp     .LExitDecRef
.LDoClassDecRef:
.LDoObjectDecRef:
.LDoRecordDecRef:
        incl    %ebx
        movzbl  (%ebx),%eax
// Skip also recordsize.
        addl    $5,%eax
        addl    %eax,%ebx
// %ebx points to element count. Set in %edx
        movl    (%ebx),%edx
        addl    $4,%ebx
// %ebx points to First element in record
.LMyRecordDecRefLoop:
        decl    %edx
        jl      .LExitDecRef
// Calculate data
        movl    8(%ebp),%eax
        addl    (%ebx),%eax
        addl     $4,%ebx
// Push type
        pushl    (%ebx)
        addl     $4,%ebx
// push data
        pushl    %eax
        call    DECREF
        jmp     .LMyRecordDecRefLoop
// Array handling
.LDoArrayDecRef:
// %ebx points to size. Put size in ecx
        movl    (%ebx),%ecx
        addl    $4, %ebx
// %ebx points to count. Put count in %edx
        movl    (%ebx),%edx
        addl    $4, %ebx
// %ebx points to type. Put into ebx.
// Start treating elements.
.LMyArrayDecRefLoop:
        decl    %edx
        jl      .LExitDecRef
// push type
        pushl   (%ebx)
// calculate data
        movl    %ecx,%eax
        imull    %edx,%eax
        addl    8(%ebp),%eax
// push data
        pushl   %eax
        call    DECREF
        jmp     .LMyArrayDecRefLoop
// AnsiString handling :
.LDoAnsiStringDecRef:
        movl    8(%ebp),%eax
        pushl   %eax
        call    ANSISTR_DECR_REF
.LExitDecRef:
        pop     %edx
        pop     %ecx
        pop     %ebx
        pop     %eax
end;

{
  $Log$
  Revision 1.18  2000-01-07 16:41:33  daniel
    * copyright 2000

  Revision 1.17  1999/07/31 22:27:28  michael
  Object finalization data fixed

  Revision 1.16  1999/03/01 15:40:58  peter
    * use external names
    * removed all direct assembler modes

  Revision 1.15  1998/12/20 14:08:06  michael
  + Finalize fixed after florians fix :)

  Revision 1.14  1998/12/19 00:22:09  florian
    * Finalize fixed, constant for ansistrings is 9, not 10

  Revision 1.13  1998/12/15 22:42:59  peter
    * removed temp symbols

  Revision 1.12  1998/11/30 10:07:34  michael
  + Adjusted typeinfo constants

  Revision 1.11  1998/11/17 00:41:10  peter
    * renamed string functions

  Revision 1.10  1998/11/16 12:21:47  peter
    * fixes for 0.99.8

  Revision 1.9  1998/09/29 08:38:25  michael
  + Corrections in record and array handling.

  Revision 1.8  1998/09/20 17:49:07  florian
    * some ansistring fixes

  Revision 1.7  1998/09/14 10:48:11  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.6  1998/08/23 20:58:50  florian
    + rtti for objects and classes
    + TObject.GetClassName implemented

  Revision 1.5  1998/06/25 08:41:43  florian
    * better rtti

  Revision 1.4  1998/06/17 11:50:43  michael
  + Small patch: forgot to make alias public

  Revision 1.3  1998/06/10 07:46:49  michael
  + Forgot to commit some changes

  Revision 1.2  1998/06/08 19:31:03  michael
  + Implemented DecRef

  Revision 1.1  1998/06/08 15:32:12  michael
  + Split rtti according to processor. Implemented optimized i386 code.

}
