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
        movl    TypeInfo,%ebx
        movb    (%ebx),%al
// This is MANIFESTLY wrong
        subb    $9,%al
        jz      .LDoAnsiStringInit
        decb    %al
        jz      .LDoAnsiStringInit
        subb    $2,%al
        jz      .LDoArrayInit
        decb    %al
        jz      .LDoRecordInit
        subb    $2,%al
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
        movl    Data,%eax
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
        addl    Data,%eax
// push data
        pushl   %eax
        call    INITIALIZE
        jmp     .LMyArrayInitLoop
// AnsiString handling :
.LDoAnsiStringInit:
        movl Data, %eax
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
        movl    TypeInfo,%ebx
        movb    (%ebx),%al
        subb    $9,%al
        jz      .LDoAnsiStringFinal
        decb    %al
        jz      .LDoAnsiStringFinal
        subb    $2,%al
        jz      .LDoArrayFinal
        decb    %al
        jz      .LDoRecordFinal
        subb    $2,%al
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
        movl    Data,%eax
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
        addl    Data,%eax
// push data
        pushl   %eax
        call    FINALIZE
        jmp     .LMyArrayFinalLoop
// AnsiString handling :
.LDoAnsiStringFinal:
        pushl   Data
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
        movl    TypeInfo,%ebx
        movb    (%ebx),%al
        subb    $9,%al
        jz      .LDoAnsiStringAddRef
        decb    %al
        jz      .LDoAnsiStringAddRef
        subb    $2,%al
        jz      .LDoArrayAddRef
        decb    %al
        jz      .LDoRecordAddRef
        subb    $2,%al
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
// Push type
        pushl    (%ebx)
        addl     $4,%ebx
// Calculate data
        movl    Data,%eax
        addl    (%ebx),%eax
        addl     $4,%ebx
// push data
        pushl    %eax
        call    ADDREF
        jmp     .LMyRecordAddRefLoop
// Array handling
.LDoArrayAddRef:
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
.LMyArrayAddRefLoop:
        decl    %edx
        jl      .LExitAddRef
// push type
        pushl   (%ebx)
// calculate data
        movl    %ecx,%eax
        imull    %edx,%eax
        addl    Data,%eax
// push data
        pushl   %eax
        call    ADDREF
        jmp     .LMyArrayAddRefLoop
// AnsiString handling :
.LDoAnsiStringAddRef:
        pushl   Data
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
        movl    TypeInfo,%ebx
        movb    (%ebx),%al
        subb    $9,%al
        jz      .LDoAnsiStringDecRef
        decb    %al
        jz      .LDoAnsiStringDecRef
        subb    $2,%al
        jz      .LDoArrayDecRef
        decb    %al
        jz      .LDoRecordDecRef
        subb    $2,%al
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
// Push type
        pushl    (%ebx)
        addl     $4,%ebx
// Calculate data
        movl    Data,%eax
        addl    (%ebx),%eax
        addl     $4,%ebx
// push data
        pushl    %eax
        call    DECREF
        jmp     .LMyRecordDecRefLoop
// Array handling
.LDoArrayDecRef:
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
.LMyArrayDecRefLoop:
        decl    %edx
        jl      .LExitDecRef
// push type
        pushl   (%ebx)
// calculate data
        movl    %ecx,%eax
        imull    %edx,%eax
        addl    Data,%eax
// push data
        pushl   %eax
        call    DECREF
        jmp     .LMyArrayDecRefLoop
// AnsiString handling :
.LDoAnsiStringDecRef:
        movl    Data,%eax
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
  Revision 1.23  2000-06-07 08:55:30  jonas
    * fixed web bug 983 (array name wasn't skipped in add/decref)

  Revision 1.22  2000/04/01 11:45:34  peter
    * fixed addref/decref for records, the data/type was read in the
      wrong order

  Revision 1.21  2000/02/18 15:23:01  florian
    * fixed constants in rtti
    * longjmp does now a finit

  Revision 1.20  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.19  2000/01/11 21:11:34  marco
   * Direct params to internal assembler

  Revision 1.18  2000/01/07 16:41:33  daniel
    * copyright 2000

  Revision 1.17  1999/07/31 22:27:28  michael
  Object finalization data fixed

}
