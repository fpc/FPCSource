{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by the Free Pascal development team.

    Portions Copyright (c) 2000 by Casey Duncan (casey.duncan@state.co.us)

    Processor dependent implementation for the system unit for
    PowerPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                                Move / Fill
****************************************************************************}

{$define FPC_SYSTEM_HAS_MOVE}

procedure Move(var source;var dest;count:longint);
begin
{ register usage:
  r3	source
  r4	dest
  r5	count
  r13	ptr to end of source
  r14	ptr to end of dest
  r15	counter 1
  r16	counter 2
  r17	addr increment
  r18	ptr to current source block
  r19	ptr to current dest block
  r20-24	buffer
  f1-4	buffer
  ctr	Loop counter
  notes:
  Move uses FPRs for increased bandwidth
}
        asm
        { do some param checking, initialization }
        cmplwi	cr2,r3,0
        cmplwi	cr3,r4,0
        cmplw	cr4,r3,r4
        add		r13,r3,r5
        add		r14,r4,r5
        bt		cr2,.MoveEnd	//end if source=nil
        bt		cr3,.MoveEnd	//end if dest=nil
        bt		cr4,.MoveEnd	//end if source=dest
        { see if source and dest overlap }
        cmplw	cr2,r13,r4
        cmplw	cr3,r4,r3
        srawi.	r15,r5,$5		//r15 := count div 32
        andi	r16,r5,$1F		//r16 := count mod 32
        crand	cr3,cr2,cr3
        mtctr	r15				//Load loop counter
        bgt		cr3,.MoveRL		//dest overlaps source on right
        li		r17,$8			//Offset 8 bytes per doubleword copy
        sub		r18,r17,r3		//calculate the starting source
        sub		r19,r17,r4		//					and dest ptrs
        beq		.MoveByByte		//If count<32 skip 32 byte block copy
        srawi.	r15,r16,$2		//r15 := r16 div 4
        andi	r16,r15,$3		//r16 := r15 mod 4
        cmpwi	cr2,r16,0		//r16 = 0 ?
        crand	cr3,cr2,cr0		//r15 = 0 AND r16 = 0 ?
.MoveBlockLoop:					//32 Byte block copy (fully optimized)
		lfdux	f1,r18,r17
		lfdux	f2,r18,r17
		lfdux	f3,r18,r17
		lfdux	f4,r18,r17
		stfdux	f1,r19,r17
		stfdux	f2,r19,r17
		stfdux	f3,r19,r17
		stfdux	f4,r19,r17
		bdnz	.MoveBlockLoop
		
		bt		cr3,MoveEnd		//Nothing left to do...
		mtspr	1,r16			//XER := r16
		beq		.MoveBytes		//There are fewer than 4 bytes left
		mtctr	r15				//load counter
		andi	r15,r15,$3		//r15 := r15 mod 4
		srawi	r17,$1			//Offset := Offset div 2
.MoveWordLoop:					//4 byte copy
		lwzux	r20,r18,r17
		stwux	r20,r19,r17
		bdnz	.WordCopyLoop

		bt		cr2,MoveEnd		//Nothing left to do...
.MoveBytes:						//Copy remaining stragglers		
		lswx	r20,r0,r18
		stswx	r20,r0,r19
.MoveEnd:
		End;
End;
		

{$define FPC_SYSTEM_HAS_FILLCHAR}

Procedure FillChar(var x;count:longint;value:byte);
begin
        asm
{ Register Usage:
	r3	x
	r4	count
	r5	value
	r13	value.value.value.value
	r14	ptr to current dest char
	r15	byte increment, Scratch
	r16	Block count
	r17 misalignment byte count
}
		cmpwi	cr2,r4,12	
		mr		r14,r3
		andi.	r17,r3,3
		sub		r14,r3,r17		//32 bit align
		blt		cr2,.FillBytes	//if count<12 then fill byte by byte
		sub		r16,r4,r17
		andi	r17,r16,3
		cmpwi	cr2,r17,0
		srwi	r16,r16,2		//r16:=count div 4
		subi	r16,r16,2
		mtctr	r16				//counter:=r16
		mr		r13,r5			//insert
		insrwi	r13,r5,8,16		//		value into all four bytes
		insrwi	r13,r13,16,0	//									of r13
		li		r15,4						
		stw		r13,0(r3)		//fill first few bytes
.FillWordLoop:
		stwux	r13,r14,r15
		bdnz	.FillWordLoop
		beq		cr2,FillEnd		//No trailing bytes, so exit
		add		r14,r3,r4
		stw		r13,-4(r14)		//fill last few bytes
		b		.FillEnd
		
.FillBytes:	
		mtctr	r4				//counter:=count
		li		r15,1
		subi	r14,r3,1
.FillByteLoop:
		stbux	r13,r14,r15
		bdnz	.FillByteLoop
.FillEnd:
        end [r13,r14,r15,r16,r17,ctr];
end;


{$define FPC_SYSTEM_HAS_FILLWORD}

procedure fillword(var x;count : longint;value : word);
begin
{	registers:
	r3		x
	r4		count
	r5		value
	r13		value.value
	r14		ptr to dest word
	r15		increment 1
	r16		increment 2
	r17		scratch
	r18		scratch
	f1		value.value.value.value
}
	asm
		cmpwi	cr0,r3,0
		andi	r17,r4,$3
		srwi	r18,r4,1	//r18:=count div 2
  		mr 		r13,r3
  		li		r14,4
  		ble		.FillWordEnd	//if count<=0 Then Exit
.FillWordLoop:
		stwux	r5,r13,r14
		bdnz	.FillWordLoop
.FillWordEnd:
	end [r13,r14,ctr]
end;

{
  $Log$
  Revision 1.1  2000-07-27 07:32:12  jonas
    + initial version by Casey Duncan (not yet thoroughly debugged or complete)

}