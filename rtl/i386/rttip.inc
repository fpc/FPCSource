{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by xxxx
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Run-Time type information routines - processor dependent part }
{$ASMMODE DIRECT}

Procedure Initialize (Data,TypeInfo : pointer);[Alias : 'INITIALIZE'];assembler;

asm
# Save registers
	push   %esp
	movl   %esp,%ebp
        push    %eax
	push    %ebx
	push    %ecx
	push    %edx
# decide what type it is
	movl	12(%ebp),%ebx
	movb	(%ebx),%al
	subb	$10,%al
	jz	.DoAnsiStringInit
	decb	%al
	jz	.DoAnsiStringInit
	subb	$2,%al
	jz	.DoArrayInit
	decb	%al
	jz	.DoRecordInit
	jmp	.ExitInitialize
.DoRecordInit:
	incl	%ebx
	movzbl	(%ebx),%eax
# Skip also recordsize.
        addl    $5,%eax 
	addl	%eax,%ebx
# %ebx points to element count. Set in %edx
	movl	(%ebx),%edx
	addl    $4,%ebx
# %ebx points to First element in record
.MyRecordInitLoop:
	decl    %edx
	jl	.ExitInitialize
# Calculate data
	movl    8(%ebp),%eax	
	addl    (%ebx),%eax
	addl     $4,%ebx
# Push type
	pushl    (%ebx)
	addl     $4,%ebx
# push data
	pushl    %eax
	call	INITIALIZE
	jmp     .MyRecordInitLoop
# Array handling
.DoArrayInit:
# %ebx points to size. Put size in ecx
	movl	(%ebx),%ecx
	addl    $4, %ebx
# %ebx points to count. Put count in %edx 
	movl	(%ebx),%edx
	addl    $4, %ebx
# %ebx points to type. Put into ebx.
# Start treating elements.
.MyArrayInitLoop:
	decl	%edx
	jl	.ExitInitialize
# push type
        pushl   (%ebx)
# calculate data
	movl    %ecx,%eax
	imull    %edx,%eax
	addl    8(%ebp),%eax
# push data
	pushl   %eax 
	call	INITIALIZE
	jmp	.MyArrayInitLoop
# AnsiString handling : 
.DoAnsiStringInit:
	movl	$0,8(%ebp)
.ExitInitialize:
        pop     %edx
	pop	%ecx
	pop	%ebx
	pop	%eax
        leave
	ret	$8
end;

Procedure Finalize (Data,TypeInfo: Pointer);[Alias : 'FINALIZE']; assembler;

asm
# Save registers
	push    %esp
	movl    %esp,%ebp
        push    %eax
	push    %ebx
	push    %ecx
	push    %edx
# decide what type it is
	movl	12(%ebp),%ebx
	movb	(%ebx),%al
	subb	$10,%al
	jz	.DoAnsiStringFinal
	decb	%al
	jz	.DoAnsiStringFinal
	subb	$2,%al
	jz	.DoArrayFinal
	decb	%al
	jz	.DoRecordFinal
	jmp	.ExitFinalize
.DoRecordFinal:
	incl	%ebx
	movzbl	(%ebx),%eax
# Skip also recordsize.
        addl    $5,%eax 
	addl	%eax,%ebx
# %ebx points to element count. Set in %edx
	movl	(%ebx),%edx
	addl    $4,%ebx
# %ebx points to First element in record
.MyRecordFinalLoop:
	decl    %edx
	jl	.ExitFinalize
# Calculate data
	movl    8(%ebp),%eax	
	addl    (%ebx),%eax
	addl     $4,%ebx
# Push type
	pushl    (%ebx)
	addl     $4,%ebx
# push data
	pushl    %eax
	call	FINALIZE
	jmp     .MyRecordFinalLoop
# Array handling
.DoArrayFinal:
# %ebx points to size. Put size in ecx
	movl	(%ebx),%ecx
	addl    $4, %ebx
# %ebx points to count. Put count in %edx 
	movl	(%ebx),%edx
	addl    $4, %ebx
# %ebx points to type. Put into ebx.
# Start treating elements.
.MyArrayFinalLoop:
	decl	%edx
	jl	.ExitFinalize
# push type
        pushl   (%ebx)
# calculate data
	movl    %ecx,%eax
	imull    %edx,%eax
	addl    8(%ebp),%eax
# push data
	pushl   %eax 
	call	FINALIZE
	jmp	.MyArrayFinalLoop
# AnsiString handling : 
.DoAnsiStringFinal:
	movl	8(%ebp),%eax
	pushl   %eax
	call    DECR_ANSI_REF
.ExitFinalize:
        pop     %edx
	pop	%ecx
	pop	%ebx
	pop	%eax
	ret	$8
end;

Procedure Addref (Data,TypeInfo : Pointer); [alias : 'ADDREF'];Assembler;

asm
# Save registers
	push    %esp
	movl    %esp,%ebp
        push    %eax
	push    %ebx
	push    %ecx
	push    %edx
# decide what type it is
	movl	12(%ebp),%ebx
	movb	(%ebx),%al
	subb	$10,%al
	jz	.DoAnsiStringAddRef
	decb	%al
	jz	.DoAnsiStringAddRef
	subb	$2,%al
	jz	.DoArrayAddRef
	decb	%al
	jz	.DoRecordAddRef
	jmp	.ExitAddRef
.DoRecordAddRef:
	incl	%ebx
	movzbl	(%ebx),%eax
# Skip also recordsize.
        addl    $5,%eax 
	addl	%eax,%ebx
# %ebx points to element count. Set in %edx
	movl	(%ebx),%edx
	addl    $4,%ebx
# %ebx points to First element in record
.MyRecordAddRefLoop:
	decl    %edx
	jl	.ExitAddRef
# Calculate data
	movl    8(%ebp),%eax	
	addl    (%ebx),%eax
	addl     $4,%ebx
# Push type
	pushl    (%ebx)
	addl     $4,%ebx
# push data
	pushl    %eax
	call	ADDREF
	jmp     .MyRecordAddRefLoop
# Array handling
.DoArrayAddRef:
# %ebx points to size. Put size in ecx
	movl	(%ebx),%ecx
	addl    $4, %ebx
# %ebx points to count. Put count in %edx 
	movl	(%ebx),%edx
	addl    $4, %ebx
# %ebx points to type. Put into ebx.
# Start treating elements.
.MyArrayAddRefLoop:
	decl	%edx
	jl	.ExitAddRef
# push type
        pushl   (%ebx)
# calculate data
	movl    %ecx,%eax
	imull    %edx,%eax
	addl    8(%ebp),%eax
# push data
	pushl   %eax 
	call	ADDREF
	jmp	.MyArrayAddRefLoop
# AnsiString handling : 
.DoAnsiStringAddRef:
	movl	8(%ebp),%eax
	pushl   %eax
	call    INCR_ANSI_REF
.ExitAddRef:
        pop     %edx
	pop	%ecx
	pop	%ebx
	pop	%eax
	leave
	ret	$8
end;

Procedure DecRef (Data,TypeInfo : Pointer); [alias : 'DECREF'];Assembler;

asm
# Save registers
	push    %esp
	movl    %esp,%ebp
        push    %eax
	push    %ebx
	push    %ecx
	push    %edx
# decide what type it is
	movl	12(%ebp),%ebx
	movb	(%ebx),%al
	subb	$10,%al
	jz	.DoAnsiStringDecRef
	decb	%al
	jz	.DoAnsiStringDecRef
	subb	$2,%al
	jz	.DoArrayDecRef
	decb	%al
	jz	.DoRecordDecRef
	jmp	.ExitDecRef
.DoRecordDecRef:
	incl	%ebx
	movzbl	(%ebx),%eax
# Skip also recordsize.
        addl    $5,%eax 
	addl	%eax,%ebx
# %ebx points to element count. Set in %edx
	movl	(%ebx),%edx
	addl    $4,%ebx
# %ebx points to First element in record
.MyRecordDecRefLoop:
	decl    %edx
	jl	.ExitDecRef
# Calculate data
	movl    8(%ebp),%eax	
	addl    (%ebx),%eax
	addl     $4,%ebx
# Push type
	pushl    (%ebx)
	addl     $4,%ebx
# push data
	pushl    %eax
	call	DECREF
	jmp     .MyRecordDecRefLoop
# Array handling
.DoArrayDecRef:
# %ebx points to size. Put size in ecx
	movl	(%ebx),%ecx
	addl    $4, %ebx
# %ebx points to count. Put count in %edx 
	movl	(%ebx),%edx
	addl    $4, %ebx
# %ebx points to type. Put into ebx.
# Start treating elements.
.MyArrayDecRefLoop:
	decl	%edx
	jl	.ExitDecRef
# push type
        pushl   (%ebx)
# calculate data
	movl    %ecx,%eax
	imull    %edx,%eax
	addl    8(%ebp),%eax
# push data
	pushl   %eax 
	call	DECREF
	jmp	.MyArrayDecRefLoop
# AnsiString handling : 
.DoAnsiStringDecRef:
	movl	8(%ebp),%eax
	pushl   %eax
	call    DECR_ANSI_REF
.ExitDecRef:
        pop     %edx
	pop	%ecx
	pop	%ebx
	pop	%eax
	leave
	ret	$8
end;

{$ASMMODE DEFAULT}

{
  $Log$
  Revision 1.3  1998-06-10 07:46:49  michael
  + Forgot to commit some changes

  Revision 1.2  1998/06/08 19:31:03  michael
  + Implemented DecRef

  Revision 1.1  1998/06/08 15:32:12  michael
  + Split rtti according to processor. Implemented optimized i386 code.

}