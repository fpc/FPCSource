{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Carl-Eric Codere,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{*************************************************************************}
{  set.inc                                                                }
{  Converted by Carl Eric Codere                                          }
{*************************************************************************}
{  This inc. implements low-level set operations for the motorola         }
{  68000 familiy of processors.                                           }
{*************************************************************************}
{  Based on original code bt Florian KlÑmpfl  for the 80x86.              }
{                                                                         }
{  22nd november 1997                                                     }
{   * bugfix of btst with long sizes. (CEC)                               }
{*************************************************************************}
  { fÅgt das Element b der Menge zu, auf die p zeigt }

  procedure do_set(p : pointer;b : byte);[public,alias: 'SET_SET_BYTE'];

    begin
       asm
          move.l 8(a6),a0
          move.b 12(a6),d6
          andi.l #$f8,d6
          lsl.l  #3,d6
          adda.l d6,a0
          move.b 12(a6),d6
          andi.l #7,d6

          move.l  (a0),d0              { we must put bits into register }
          btst.l  d6,d0                { otherwise btst will be a byte  }
          { put result in carry flag } { operation.                     }
          bne    @LDOSET1
          andi.b #$fe,ccr              { clear carry flag }
          bra    @LDOSET2
       @LDOSET1:
          ori.b  #$01,ccr              { set carry flag   }
       @LDOSET2:
       end ['d0','a0','d6'];
    end;

  { testet, ob das Element b in der Menge p vorhanden ist }
  { und setzt das Carryflag entsprechend                  }

  procedure do_in(p : pointer;b : byte);[public,alias: 'SET_IN_BYTE'];
  { Returns Carry set then = in set , otherwise carry is cleared }
    begin
      asm
          move.l  8(a6),a0
          move.b  12(a6),d6
          andi.l  #$f8,d6
          lsl.l   #3,d6
          adda.l  d6,a0       { correct offset from start address of set }

          move.b 12(a6),d6
          andi.l #7,d6

          move.l  (a0),d0              { we must put bits into register }
          btst.l  d6,d0                { otherwise btst will be a byte  }
          { put result in carry flag } { operation.                     }
          bne    @LDOIN1
          andi.b #$fe,ccr              { clear carry flag }
          bra    @LDOIN2
       @LDOIN1:
          ori.b  #$01,ccr              { set carry flag   }
       @LDOIN2:
       end ['d0','a0','d6'];
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

           move.l #8,d6

       @LMADDSETS1:

           move.l  (a0)+,d0
           or.l    (a1)+,d0
           move.l  d0,(a2)+
           subq.l  #4,d6
           bne     @LMADDSETS1
           { restore register }
           move.l  a2,(sp)+
        end ['d0','d6','a0','a1'];
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

           move.l #8,d6

       @LMMULSETS1:

           move.l  (a0)+,d0
           and.l    (a1)+,d0
           move.l  d0,(a2)+
           subq.l  #4,d6
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

           move.l #8,d6

       @LSUBSETS1:

           move.l  (a0)+,d0
           not.l   d0
           and.l   (a1)+,d0
           move.l  d0,(a2)+
           subq.l  #4,d6
           bne     @LSUBSETS1
           { restore register }
           move.l  a2,(sp)+
        end ['d0','d6','a0','a1'];
     end;

   { vergleicht Mengen und setzt die Flags entsprechend }

   procedure comp_sets(set1,set2 : pointer);[public,alias: 'SET_COMP_SETS'];

     begin
        asm
           move.l 8(a6),a0  { set1 - esi}
           move.l 12(a6),a1 { set2 - edi }
           move.l #8,d6
       @LMCOMPSETS1:
           move.l (a0)+,d0
           move.l (a1),d1
           cmp.l  d1,d0
           bne  @LMCOMPSETEND
           add.l #4,a1
           subq.l #1,d6
           bne  @LMCOMPSETS1
           { we are here only if the two sets are equal         }
           { we have zero flag set, and that what is expected   }
           cmp.l d0,d0
       @LMCOMPSETEND:
        end;
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

{
  $Log$
  Revision 1.1  1998-03-25 11:18:44  root
  Initial revision

  Revision 1.4  1998/01/26 12:01:42  michael
  + Added log at the end


  
  Working file: rtl/m68k/set.inc
  description:
  ----------------------------
  revision 1.3
  date: 1998/01/05 00:34:50;  author: carl;  state: Exp;  lines: +349 -350
  * Silly syntax errors fixed.
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:37:22;  author: michael;  state: Exp;  lines: +14 -0
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/28 16:51:20;  author: carl;  state: Exp;
  + set routines for m68k.
  =============================================================================
}
