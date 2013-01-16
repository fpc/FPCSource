{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl-Eric Codere,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{*************************************************************************}
{  Converted by Carl Eric Codere                                          }
{*************************************************************************}
{  This inc. implements low-level set operations for the motorola         }
{  68000 familiy of processors.                                           }
{  Based on original code bt Florian Kl„mpfl  for the 80x86.              }
{*************************************************************************}

{ Edit by Sven: No m68k specific set routines for now (doesn't seem to be
                compatible to the way FPC handles sets today anyway...) }

{$ifdef use_m68k_sets}
  { add the element b to the set pointed by p }
  { On entry                                   }
  {  a0    = pointer to set                    }
  {  d0.b  = element to add to the set         }
  { Registers destroyed: d0,a1,d6              }
  procedure do_set;assembler;
  asm
    XDEF SET_SET_BYTE
          move.l  d0,d6
          { correct long position: }
          {   -> (value div 32)*4 = longint }
          {       (value shr 5)*shl 2       }
          lsr.l  #5,d6
          lsl.l  #2,d6
          adda.l d6,a0       { correct offset from start address of set }

          move.l d0,d6       { bit is now in here                       }
          andi.l #31,d0      { bit number is =  value mod 32            }

          { now bit set the value }
          move.l  (a0),d0              { we must put bits into register }
          bset.l  d6,d0                { otherwise btst will be a byte  }
          { put result in carry flag } { operation.                     }
          bne    @LDOSET1
          andi.b #$fe,ccr              { clear carry flag }
          bra    @LDOSET2
       @LDOSET1:
          ori.b  #$01,ccr              { set carry flag   }
       @LDOSET2:
          move.l  d0,(a0)              { restore the value at that location }
                                       { of the set.                        }
    end;

    { Finds an element in a set }
    { a0   = address of set                                 }
    { d0.b = value to compare with                          }
    { CARRY SET IF FOUND ON EXIT                            }
    { Registers destroyed: d0,a0,d6                         }
  procedure do_in; assembler;
  { Returns Carry set then = in set , otherwise carry is cleared }
  {         (D0)                                                 }
  asm
       XDEF SET_IN_BYTE
          move.l  d0,d6
          { correct long position: }
          {   -> (value div 32)*4 = longint }
          {       (value shr 5)*shl 2       }
          lsr.l  #5,d6
          lsl.l  #2,d6
          adda.l d6,a0       { correct offset from start address of set }

          move.l d0,d6       { bit is now in here                       }
          andi.l #31,d0      { bit number is =  value mod 32            }

          move.l  (a0),d0              { we must put bits into register }
          btst.l  d6,d0                { otherwise btst will be a byte  }
          { put result in carry flag } { operation.                     }
          bne    @LDOIN1
          andi.b #$fe,ccr              { clear carry flag }
          bra    @LDOIN2
       @LDOIN1:
          ori.b  #$01,ccr             { set carry flag   }
       @LDOIN2:
    end;



   { vereinigt set1 und set2 und speichert das Ergebnis in dest }

   procedure add_sets(set1,set2,dest : pointer);[public,alias: 'SET_ADD_SETS'];
   {  PSEUDO-CODE:
       type
         destination = array[1..8] of longint;
        for i:=1 to 8 do
           destination(dest^)[i] := destination(set1^)[i] OR destination(set2^)[i];
    }
    begin
        asm
           { saved used register }
           move.l a2,-(sp)

           move.l 8(a6),a0
           move.l 12(a6),a1
           move.l 16(a6),a2

           move.l #32,d6

       @LMADDSETS1:

           move.b  (a0)+,d0
           or.b    (a1)+,d0
           move.b  d0,(a2)+
           subq.b  #1,d6
           bne     @LMADDSETS1
           { restore register }
           move.l  a2,(sp)+
        end ['d0','d6','a0','a1'];
     end;

   { computes the symetric diff from set1 to set2    }
   { result in dest                                  }

   procedure sym_sub_sets(set1,set2,dest : pointer);[public,alias: 'SET_SYMDIF_SETS'];

     begin
        asm
           { saved used register }
           move.l a2,-(sp)

           move.l 8(a6),a0
           move.l 12(a6),a1
           move.l 16(a6),a2

           move.l #32,d6

       @LMADDSETS1:

           move.b  (a0)+,d0
           move.b  (a1)+,d1
           eor.b   d1,d0
           move.b  d0,(a2)+
           subq.b  #1,d6
           bne     @LMADDSETS1
           { restore register }
           move.l  a2,(sp)+
        end;
     end;


  { bad implementation, but it's very seldom used }
  procedure do_set(p : pointer;l,h : byte);[public,alias: 'SET_SET_RANGE'];

    begin
       asm
          move.b h,d0
       @LSetRLoop:
          cmp.b  l,d0
          blt    @Lend
          move.w d0,-(sp)
          { adjust value to correct endian }
          lsl.w  #8,d0
          pea    p
//          jsr    SET_SET_BYTE
          sub.b  #1,d0
          bra    @LSetRLoop
       @Lend:
       end;
    end;


   { bildet den Durchschnitt von set1 und set2 }
   { und speichert das Ergebnis in dest        }

   procedure mul_sets(set1,set2,dest : pointer);[public,alias: 'SET_MUL_SETS'];
   {  type
         larray = array[0..7] of longint;
        for i:=0 to 7 do
           larray(dest^)[i] := larray(set1^)[i] AND larray(set2^)[i];
   }
   begin
        asm
           { saved used register }
           move.l a2,-(sp)
           move.l 8(a6),a0
           move.l 12(a6),a1
           move.l 16(a6),a2

           move.l #32,d6

       @LMMULSETS1:

           move.b  (a0)+,d0
           and.b   (a1)+,d0
           move.b  d0,(a2)+
           subq.b  #1,d6
           bne     @LMMULSETS1
           { restore register }
           move.l  a2,(sp)+
        end ['d0','d6','a0','a1'];
     end;


   { bildet die Differenz von set1 und set2 }
   { und speichert das Ergebnis in dest     }

   procedure sub_sets(set1,set2,dest : pointer);[public,alias: 'SET_SUB_SETS'];
   {  type
         larray = array[0..7] of longint;
     begin
        for i:=0 to 7 do
           larray(dest^)[i] := larray(set1^)[i] AND NOT (larray(set2^)[i]);
     end;
     }
   begin
        asm
           { saved used register }
           move.l a2,-(sp)
           move.l 8(a6),a0
           move.l 12(a6),a1
           move.l 16(a6),a2

           move.l #32,d6

       @LSUBSETS1:
           move.b  (a0)+,d0
           move.b  (a1)+,d1
           not.b   d1
           and.b   d1,d0
           move.b  d0,(a2)+
           sub.b   #1,d6
           bne     @LSUBSETS1
           { restore register }
           move.l  a2,(sp)+
        end ['d0','d1','d6','a0','a1'];
     end;

   { compare both sets }
   { compares set1 and set2                             }
   { zeroflag is set if they are equal                  }
   { on entry :  a0 = pointer to first set              }
   {          :  a1 = pointer to second set             }
   procedure comp_sets; assembler;

        asm
         XDEF SET_COMP_SETS
           move.l #32,d6
       @LMCOMPSETS1:
           move.b (a0)+,d0
           move.b (a1),d1
           cmp.b  d1,d0
           bne    @LMCOMPSETEND
           adda.l #1,a1
           sub.b  #1,d6
           bne    @LMCOMPSETS1
           { we are here only if the two sets are equal         }
           { we have zero flag set, and that what is expected   }
           cmp.b  d0,d0
       @LMCOMPSETEND:
     end;

  procedure do_set(p : pointer;b : word);[public,alias: 'SET_SET_WORD'];
  begin
     asm
          move.l 8(a6),a0
          move.w 12(a6),d6
          andi.l #$fff8,d6
          lsl.l  #3,d6
          adda.l d6,a0
          move.b 12(a6),d6
          andi.l #7,d6

          move.l  (a0),d0              { we must put bits into register }
          btst.l  d6,d0                { otherwise btst will be a byte  }
          { put result in carry flag } { operation.                     }
          bne    @LBIGDOSET1
          andi.b #$fe,ccr              { clear carry flag }
          bra    @LBIGDOSET2
       @LBIGDOSET1:
          ori.b  #$01,ccr              { set carry flag   }
       @LBIGDOSET2:
       end ['d0','a0','d6'];
  end;

  { testet, ob das Element b in der Menge p vorhanden ist }
  { und setzt das Carryflag entsprechend                  }

  procedure do_in(p : pointer;b : word);[public,alias: 'SET_IN_WORD'];
    begin
      asm
          move.l  8(a6),a0
          move.w  12(a6),d6
          andi.l  #$fff8,d6
          lsl.l   #3,d6
          adda.l  d6,a0       { correct offset from start address of set }

          move.b 12(a6),d6
          andi.l #7,d6

          move.l  (a0),d0              { we must put bits into register }
          btst.l  d6,d0                { otherwise btst will be a byte  }
          { put result in carry flag } { operation.                     }
          bne    @LBIGDOIN1
          andi.b #$fe,ccr              { clear carry flag }
          bra    @LBIGDOIN2
       @LBIGDOIN1:
          ori.b  #$01,ccr              { set carry flag   }
       @LBIGDOIN2:
       end ['d0','a0','d6'];
    end;


   { vereinigt set1 und set2 und speichert das Ergebnis in dest }
   { size is the number of bytes in the set }

   procedure add_sets(set1,set2,dest : pointer;size : longint);[public,alias: 'SET_ADD_SETS_SIZE'];
    begin
        asm
           { saved used register }
           move.l a2,-(sp)
           move.l 8(a6),a0
           move.l 12(a6),a1
           move.l 16(a6),a2

           move.l 20(a6),d6

       @LBIGMADDSETS1:

           move.l  (a0)+,d0
           or.l    (a1)+,d0
           move.l  d0,(a2)+
           subq.l  #4,d6
           bne     @LBIGMADDSETS1
           { restore register }
           move.l  a2,(sp)+
        end ['d0','d6','a0','a1'];
     end;


   procedure mul_sets(set1,set2,dest : pointer;size : longint);[public,alias: 'SET_MUL_SETS_SIZE'];
   { bildet den Durchschnitt von set1 und set2 }
   { und speichert das Ergebnis in dest        }
   { size is the number of bytes in the set }
   begin
        asm
           { saved used register }
           move.l a2,-(sp)
           move.l 8(a6),a0
           move.l 12(a6),a1
           move.l 16(a6),a2

           move.l 20(a6),d6

       @LBIGMMULSETS1:

           move.l  (a0)+,d0
           and.l    (a1)+,d0
           move.l  d0,(a2)+
           subq.l  #4,d6
           bne     @LBIGMMULSETS1
           { restore register }
           move.l  a2,(sp)+
        end ['d0','d6','a0','a1'];
     end;


   { bildet die Differenz von set1 und set2 }
   { und speichert das Ergebnis in dest     }
   { size is the number of bytes in the set }

   procedure sub_sets(set1,set2,dest : pointer;size : longint);[public,alias: 'SET_SUB_SETS_SIZE'];
   begin
        asm
           { saved used register }
           move.l a2,-(sp)
           move.l 8(a6),a0
           move.l 12(a6),a1
           move.l 16(a6),a2

           move.l 20(a6),d6

       @BIGSUBSETS1:

           move.l  (a0)+,d0
           not.l   d0
           and.l   (a1)+,d0
           move.l  d0,(a2)+
           subq.l  #4,d6
           bne     @BIGSUBSETS1
           { restore register }
           move.l  a2,(sp)+
        end ['d0','d6','a0','a1'];
     end;


   { vergleicht Mengen und setzt die Flags entsprechend }

   procedure comp_sets(set1,set2 : pointer;size : longint);[public,alias: 'SET_COMP_SETS_SIZE'];


     begin
        asm
           move.l 8(a6),a0  { set1 - esi}
           move.l 12(a6),a1 { set2 - edi }
           move.l 16(a6),d6
       @MCOMPSETS1:
           move.l (a0)+,d0
           move.l (a1),d1
           cmp.l  d1,d0
           bne  @BIGMCOMPSETEND
           add.l #4,a1
           subq.l #1,d6
           bne  @MCOMPSETS1
           { we are here only if the two sets are equal         }
           { we have zero flag set, and that what is expected   }
           cmp.l d0,d0
       @BIGMCOMPSETEND:
        end;
     end;
{$endif}
