{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Florian Klamepfl,
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  History:
  30.3.1994: Version 0.5
             Unit ist elementar implementiert
             (INT $33 Funktionen 0-3)
  2.4.1995:  Version 0.55
             - mousereset in eine function umgewandelt
             - mousesetpos implementiert
             - mouserelx, mouserely implementiert
             - mousex und mousey erweitern nun ihre Resultate
               immer auf 32 Bit
  14.4.1995: Version 0.56
             - mouserelx und mouserely mÅssen ihre
               Resultate natÅrlich Vorzeichen erweitert auf
               32 Bit kopieren
            - mouserelx und mouserely zu einer Funktion zusammen-
              gefa·t, da sonst viele Bewegungen verloren gehen
}

{$E-}

unit mouse;

{$I os.inc}

  interface
  
    function mousereset : word;
    procedure mouseon;
    procedure mouseoff;
    function mousex : longint;
    function mousey : longint;
    procedure mouserel(var x,y : longint);
    function mousebuttons : longint;
    procedure mousesetpos(x,y : longint);
  
  implementation
  
    function mousereset : word;

      begin
         asm
            movw $0,%ax
            pushl %ebp
            int	 $0x33
            popl %ebp
            leave
            ret
         end;
      end;

   procedure mouseon;

     begin
         asm
            movw $1,%ax
            pushl %ebp
            int	 $0x33
            popl %ebp
         end;
     end;

   procedure mouseoff;

     begin
        asm
           movw $2,%ax
           pushl %ebp
           int	 $0x33
           popl %ebp
        end;
     end;
     
   function mousex : longint;

     begin
        asm
           movw $3,%ax
           pushl %ebp
           int	 $0x33
           popl %ebp
           movzwl %cx,%eax
           leave
           ret
        end;
     end;
     
   function mousey : longint;

     begin
        asm
           movw $3,%ax
           pushl %ebp
           int	 $0x33
           popl %ebp
           movzwl %dx,%eax
           leave
           ret
        end;
     end;

   function mousebuttons : longint;

     begin
        asm
           movw $3,%ax
           pushl %ebp
           int	 $0x33
           popl %ebp
           movl %ebx,%eax
           andl $7,%eax
           leave
           ret
        end;
     end;
     
   procedure mousesetpos(x,y : longint);
   
     begin
        asm
           movw $4,%ax
           movl 8(%ebp),%ecx
           movl 12(%ebp),%edx
           pushl %ebp
           int	 $0x33
           popl %ebp
        end;
     end;
     
   procedure mouserel(var x,y : longint);

     begin
        asm
           movw $11,%ax
           pushl %ebp
           int	 $0x33
           popl %ebp
           movswl %cx,%ecx
           movl 8(%ebp),%eax
           movl %ecx,(%eax)
           movswl %dx,%edx
           movl 12(%ebp),%eax
           movl %edx,(%eax)
        end;
     end;
     
end.

{
  $Log$
  Revision 1.1.1.1  1998-03-25 11:18:41  root
  * Restored version

  Revision 1.3  1998/01/26 11:56:50  michael
  + Added log at the end


  
  Working file: rtl/dos/mouse.pp
  description:
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:15:47;  author: michael;  state: Exp;  lines: +12 -5
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:50;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:50;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
