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
{ I think we should use the pascal version, this code isn't     }
{ much faster                                                   }

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
        decb    %al
        jz      .LDoVariantInit
        decb    %al
        jz      .LDoArrayInit
        decb    %al
        jz      .LDoRecordInit
        decb    %al
        jz      .LDoInterfaceInit
        decb    %al
        jz      .LDoClassInit
        decb    %al
        jz      .LDoObjectInit
        decb    %al
        // what is called here ??? FK
        jz      .LDoClassInit
        subb    $4,%al
        jz      .LDoDynArrayInit
        jmp     .LExitInitialize
        // Interfaces
.LDoInterfaceInit:
        movl Data, %eax
        movl $0,(%eax)
        jmp     .LExitInitialize
        // Variants
.LDoVariantInit:
        jmp     .LExitInitialize
        // dynamic Array
.LDoDynArrayInit:
        movl Data, %eax
        movl $0,(%eax)
        jmp    .LExitInitialize
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
        call    Initialize
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
        call    Initialize
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


Procedure int_finalize (Data,TypeInfo: Pointer);[Public,Alias:'FPC_FINALIZE'];assembler;
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
        decb    %al
        jz      .LDoVariantFinal
        decb    %al
        jz      .LDoArrayFinal
        decb    %al
        jz      .LDoRecordFinal
        decb    %al
        jz      .LDoInterfaceFinal
        decb    %al
        jz      .LDoClassFinal
        decb    %al
        jz      .LDoObjectFinal
        decb    %al
        // what is called here ??? FK
        jz      .LDoClassFinal
        subb    $4,%al
        jz      .LDoDynArrayFinal
        jmp     .LExitFinalize
        // Interfaces
.LDoInterfaceFinal:
        pushl   Data
        call    Int_Intf_Decr_Ref
        jmp     .LExitFinalize
        // Variants
.LDoVariantFinal:
        jmp     .LExitFinalize
        // dynamic Array
.LDoDynArrayFinal:
        pushl   TypeInfo
        pushl   Data
        call    DYNARRAY_DECR_REF
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
        call    INT_FINALIZE
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
        call    INT_FINALIZE
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
        decb    %al
        jz      .LDoVariantAddRef
        decb    %al
        jz      .LDoArrayAddRef
        decb    %al
        jz      .LDoRecordAddRef
        decb    %al
        jz      .LDoInterfaceAddRef
        decb    %al
        jz      .LDoClassAddRef
        decb    %al
        jz      .LDoObjectAddRef
        decb    %al
        // what is called here ??? FK
        jz      .LDoClassAddRef
        subb    $4,%al
        jz      .LDoDynArrayAddRef
        jmp     .LExitAddRef
        // Interfaces
.LDoInterfaceAddRef:
        pushl   Data
        call    INT_INTF_INCR_REF
        jmp     .LExitAddRef
        // Variants
.LDoVariantAddRef:
        jmp     .LExitAddRef
        // Dynamic Arrays
.LDoDynArrayAddRef:
        movl    Data,%eax
        orl     %eax,%eax
        jz      .LExitAddRef
        lock
        incl    -8(%eax)
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
        decb    %al
        jz      .LDoVariantDecRef
        decb    %al
        jz      .LDoArrayDecRef
        decb    %al
        jz      .LDoRecordDecRef
        decb    %al
        jz      .LDoInterfaceDecRef
        decb    %al
        jz      .LDoClassDecRef
        decb    %al
        jz      .LDoObjectDecRef
        decb    %al
        // what is called here ??? FK
        jz      .LDoClassDecRef
        subb    $4,%al
        jz      .LDoDynArrayDecRef
        jmp     .LExitDecRef
        // Interfaces
.LDoInterfaceDecRef:
        pushl   Data
        call    INT_INTF_DECR_REF
        jmp     .LExitDecRef
        // Variants
.LDoVariantDecRef:
        jmp     .LExitDecRef
        // Dynamic Arrays
.LDoDynArrayDecRef:
        pushl   TypeInfo
        pushl   Data
        call    DYNARRAY_DECR_REF
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

procedure FinalizeArray(data,typeinfo : pointer;count,size : longint);
  [Public,Alias:'FPC_FINALIZEARRAY'];

  var
     i : longint;

  begin
     for i:=0 to count-1 do
       int_finalize(data+size*i,typeinfo);
  end;

{
  $Log$
  Revision 1.7  2000-11-09 17:49:34  florian
    + FPC_FINALIZEARRAY
    * Finalize to int_finalize renamed

  Revision 1.6  2000/11/06 21:52:21  florian
    * another fix for interfaces

  Revision 1.5  2000/11/06 21:35:59  peter
    * removed some warnings

  Revision 1.4  2000/11/04 16:30:35  florian
    + interfaces support

  Revision 1.3  2000/10/21 18:20:17  florian
    * a lot of small changes:
       - setlength is internal
       - win32 graph unit extended
       ....

  Revision 1.2  2000/07/13 11:33:41  michael
  + removed logs
}
