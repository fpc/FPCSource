{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************

           i386.inc : Processor dependent implementation of the system unit
                      for the Intel Ix86, x>=3

 ****************************************************************************}

    procedure int_help_constructor;

      begin
         asm
.globl HELP_CONSTRUCTOR_NE
{$IFDEF LINUX}
  .type HELP_CONSTRUCTOR_NE,@function
{$ENDIF}
HELP_CONSTRUCTOR_NE:

.globl HELP_CONSTRUCTOR
{$IFDEF LINUX}
  .type HELP_CONSTRUCTOR,@function
{$ENDIF}
HELP_CONSTRUCTOR:
      { Entry without preamble, since we need the ESP of the
        constructor }
      { Stack (relative to %ebp):
          12 Self
          8 VMT-Address
          4 main programm-Addr
          0 %ebp
      }
      {eax isn't touched anywhere, so it doesn't have to reloaded}
      movl 8(%ebp),%eax
      { initialise self ? }
      orl %esi,%esi
      jne .LHC_4
      { get memory, but save register first }
      { temporary variable }
      subl $4,%esp
      movl %esp,%esi
      { Save Register}
      pushal
      { Memory size }
      pushl (%eax)
      pushl %esi
      call GETMEM
      popal
      { Memory size to %esi }
      movl (%esi),%esi
      addl $4,%esp
      { If no memory available : fail() }
      orl %esi,%esi
      jz .LHC_5
      { init self for the constructor }
      movl %esi,12(%ebp)
   .LHC_4:
      { is there a VMT address ? }
      orl %eax,%eax
      jnz .LHC_7
      { In case the constructor doesn't do anything, the Zero-Flag }
      { can't be put, because this calls Fail() }
      incl %eax
      ret
   .LHC_7:
      { set zero inside the object }
      pushal
      pushw $0
      pushl (%eax)
      pushl %esi
      {                }
      call .L_FILL_OBJECT
      popal
      { set the VMT address for the new created object }
      movl %eax,(%esi)
      orl %eax,%eax
   .LHC_5:
      ret
   end;
end;

procedure help_fail;

  begin
     asm
     end;
  end;

procedure int_new_class;assembler;

  asm
  .global NEW_CLASS
{$IFDEF LINUX}
  .type NEW_CLASS,@function
{$ENDIF}
  NEW_CLASS:
     { create class ? }
     movl 8(%ebp),%edi
     orl %edi,%edi
     jz .LNEW_CLASS1
     { esi contains vmt }
     pushl %esi
     { call newinstance (class method!) }
     call 16(%esi)
     { new instance returns a pointer to the new created }
     { instance in eax                                   }
     { load esi and insert self                          }
     movl %eax,8(%ebp)
     movl %eax,%esi
     ret
.LNEW_CLASS1:
     movl %esi,8(%ebp)
     ret
  end;

procedure int_dispose_class;assembler;

  asm
  .global DISPOSE_CLASS
{$IFDEF LINUX}
  .type DISPOSE_CLASS,@function
{$ENDIF}
  DISPOSE_CLASS:
     { destroy class ? }
     movl 8(%ebp),%edi
     { save self }
     movl %esi,8(%ebp)
     orl %edi,%edi
     jz .LDISPOSE_CLASS1
     { no inherited call }
     movl (%esi),%edi
     { push self }
     pushl %esi
     { call freeinstance }
     call 20(%edi)
.LDISPOSE_CLASS1:
     { load self }
     movl 8(%ebp),%esi
     ret
  end;

{ checks for a correct vmt pointer }
procedure co;assembler;

  asm
  .globl CHECK_OBJECT
{$IFDEF LINUX}
  .type CHECK_OBJECT,@function
{$ENDIF}
  CHECK_OBJECT:
     pushl %edi
     movl 8(%esp),%edi
     pushl %eax
     { Here we must check if the VMT pointer is nil before  }
     { accessing it...                                      }
     { WARNING: Will only probably work with GAS, as fields }
     { are ZEROED automatically in BSS, which might not be  }
     { the case with other linkers/assemblers...            }
     orl   %edi,%edi
     jz    .Lco_re
     movl (%edi),%eax
     addl 4(%edi),%eax
     jnz .Lco_re
     popl %eax
     popl %edi
     ret $4
.Lco_re:
     pushw $210
     call runerror
  end;

procedure int_help_destructor;

  begin
     asm
        { Stack (relative to %ebp):
            12 Self
            8 VMT-Address
            4 Main program-Addr
            0 %ebp
        }
.globl HELP_DESTRUCTOR_NE
{$IFDEF LINUX}
  .type HELP_DESTRUCTOR_NE,@function
{$ENDIF}
HELP_DESTRUCTOR_NE:
.globl HELP_DESTRUCTOR
{$IFDEF LINUX}
  .type HELP_DESTRUCTOR,@function
{$ENDIF}
HELP_DESTRUCTOR:
      { temporary Variable }
      subl $4,%esp
      movl %esp,%edi
      pushal
      { Should the object be resolved ? }
      movl 8(%ebp),%eax
      orl %eax,%eax
      jz .LHD_3
      { Yes, get size from SELF! }
      movl 12(%ebp),%eax
      { get VMT-pointer (from Self) to %ebx }
      movl (%eax),%ebx
      { And put size on the Stack }
      pushl (%ebx)
      { SELF }
      { I think for precaution }
      { that we should clear the VMT here }
      movl $0,(%eax)
      movl %eax,(%edi)
      pushl %edi
      call FREEMEM
   .LHD_3:
      popal
      addl $4,%esp
      ret
   end;
end;

function get_addr(BP : longint) : longint;

  begin
     asm
        movl BP,%eax
        cmpl $0,%eax
        je .Lnul_address
        movl 4(%eax),%eax
     .Lnul_address:
        movl %eax,__RESULT
     end ['EAX'];
  end;

function get_next_frame(bp : longint) : longint;

  begin
     asm
        movl bp,%eax
        cmpl $0,%eax
        je .Lnul_frame
        movl (%eax),%eax
     .Lnul_frame:
        movl %eax,__RESULT
     end ['EAX'];
  end;

procedure runerror(w : word);[alias: 'runerror'];

  function get_addr : longint;

    begin
       asm
          movl (%ebp),%eax
          movl 4(%eax),%eax
          movl %eax,__RESULT
       end ['EAX'];
    end;

  function get_error_bp : longint;

    begin
       asm
          movl (%ebp),%eax {%ebp of run_error}
          movl %eax,__RESULT
       end ['EAX'];
    end;

  begin
     errorcode:=w;
     exitcode:=w;
     erroraddr:=pointer(get_addr);
     errorbase:=get_error_bp;
     doError:=True;
     halt(errorcode);
  end;

procedure io1(addr : longint);[public,alias: 'IOCHECK'];

  var
     l : longint;

  begin
     { Since IOCHECK is called directly and only later the optimiser }
     { Maybe also save global registers  }
     asm
        pushal
     end;
     l:=ioresult;
     if l<>0 then
       begin
          writeln('IO-Error ',l,' at ',addr);
          halt(l);
       end;
     asm
        popal
     end;
  end;

procedure re_overflow;[public,alias: 'RE_OVERFLOW'];

  var
     addr : longint;

  begin
     { Overflow was shortly before the return address }
     asm
        movl 4(%ebp),%edi
        movl %edi,addr
     end;
     writeln('Overflow at ',addr);
     RunError(215);
  end;

{ this procedure must save all modified registers except EDI and ESI !!! }
procedure strcopy(dstr,sstr : pointer;len : longint);[public,alias: 'STRCOPY'];
begin
  asm
        pushl %eax
        pushl %ecx
        cld
        movl    16(%ebp),%edi
        movl    12(%ebp),%esi
        xorl    %eax,%eax
        movl    8(%ebp),%ecx
        lodsb
        cmpl    %ecx,%eax
        jbe     .LStrCopy1
        movl    %ecx,%eax
.LStrCopy1:
        stosb
        cmpl    $7,%eax
        jl      .LStrCopy2
        movl    %edi,%ecx       # Align on 32bits
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        rep
        movsb
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        rep
        movsl
.LStrCopy2:
        movl    %eax,%ecx
        rep
        movsb
        popl %ecx
        popl %eax
  end ['ECX','EAX','ESI','EDI'];
end;


procedure strconcat(s1,s2 : pointer);[public,alias: 'STRCONCAT'];
begin
  asm
        xorl    %ecx,%ecx
        movl    12(%ebp),%edi
        movl    8(%ebp),%esi
        movl    %edi,%ebx
        movb    (%edi),%cl
        lea     1(%edi,%ecx),%edi
        negl    %ecx
        xor     %eax,%eax
        addl    $0xff,%ecx
        lodsb
        cmpl    %ecx,%eax
        jbe     .LStrConcat1
        movl    %ecx,%eax
.LStrConcat1:
        addb    %al,(%ebx)
        cmpl    $7,%eax
        jl      .LStrConcat2
        movl    %edi,%ecx       # Align on 32bits
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        rep
        movsb
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        rep
        movsl
.LStrConcat2:
        movl    %eax,%ecx
        rep
        movsb
  end ['EBX','ECX','EAX','ESI','EDI'];
end;


procedure strcmp(dstr,sstr : pointer);[public,alias: 'STRCMP'];
begin
  asm
        cld
        xorl    %ebx,%ebx
        xorl    %eax,%eax
        movl    12(%ebp),%esi
        movl    8(%ebp),%edi
        movb    (%esi),%al
        movb    (%edi),%bl
        movl    %eax,%edx
        incl    %esi
        incl    %edi
        cmpl    %ebx,%eax
        jbe     .LStrCmp1
        movl    %ebx,%eax
.LStrCmp1:
        cmpl    $7,%eax
        jl      .LStrCmp2
        movl    %edi,%ecx       # Align on 32bits
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        orl     %ecx,%ecx
        rep
        cmpsb
        jne     .LStrCmp3
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        orl     %ecx,%ecx
        rep
        cmpsl
        je      .LStrCmp2
        movl    $4,%eax
        sub     %eax,%esi
        sub     %eax,%edi
.LStrCmp2:
        movl    %eax,%ecx
        orl     %eax,%eax
        rep
        cmpsb
        jne     .LStrCmp3
        cmp     %ebx,%edx
.LStrCmp3:
  end ['EDX','ECX','EBX','EAX','ESI','EDI'];
end;


function strpas(p : pchar) : string;
begin
  asm
        cld
        movl    12(%ebp),%edi
        movl    $0xff,%ecx
        xorl    %eax,%eax
        movl    %edi,%esi
        repne
        scasb
        movl    %ecx,%eax

        movl    8(%ebp),%edi
        notb    %al
        decl    %eax
        stosb
        cmpl    $7,%eax
        jl      .LStrPas2
        movl    %edi,%ecx       # Align on 32bits
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        rep
        movsb
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        rep
        movsl
.LStrPas2:
        movl    %eax,%ecx
        rep
        movsb
  end ['ECX','EAX','ESI','EDI'];
end;


function strlen(p : pchar) : longint;
begin
  asm
        cld
        movl    8(%ebp),%edi
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        repne
        scasb
        movl    $0xfffffffe,%eax
        subl    %ecx,%eax
        leave
        ret     $4
  end ['EDI','ECX','EAX'];
end;


procedure Move(var source;var dest;count:longint);
begin
        asm
        movl    dest,%edi
        movl    source,%esi
        movl    %edi,%eax
        movl    count,%ebx
## Check for back or forward
        sub     %esi,%eax
        jz      .LMoveEnd               # Do nothing when source=dest
        jc      .LFMove                 # Do forward, dest<source
        cmp     %ebx,%eax
        jb      .LBMove                 # Dest is in range of move, do backward
## Forward Copy
.LFMove:
        cld
        cmpl    $7,%ebx
        jl      .LFMove1
        movl    %edi,%ecx       # Align on 32bits
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%ebx
        rep
        movsb
        movl    %ebx,%ecx
        andl    $3,%ebx
        shrl    $2,%ecx
        rep
        movsl
.LFMove1:
        movl    %ebx,%ecx
        rep
        movsb
        jmp .LMoveEnd
## Backward Copy
.LBMove:
        std
        addl    %ebx,%esi
        addl    %ebx,%edi
        movl    %edi,%ecx
        decl    %esi
        decl    %edi
        cmpl    $7,%ebx
        jl      .LBMove1
        negl    %ecx            # Align on 32bits
        andl    $3,%ecx
        subl    %ecx,%ebx
        rep
        movsb
        movl    %ebx,%ecx
        andl    $3,%ebx
        shrl    $2,%ecx
        subl    $3,%esi
        subl    $3,%edi
        rep
        movsl
        addl    $3,%esi
        addl    $3,%edi
.LBMove1:
        movl    %ebx,%ecx
        rep
        movsb
        cld
.LMoveEnd:
        end;
end;


Procedure FillChar(var x;count:longint;value:byte);[alias: '.L_FILL_OBJECT'];
begin
        asm
        cld
        movl    x,%edi
        movl    value,%eax      # Only lower 8 bits will be used
        movl    count,%ecx
        cmpl    $7,%ecx
        jl      .LFill1
        movb    %al,%ah
        movl    %eax,%ebx
        shll    $16,%eax
        movl    %ecx,%edx
        movw    %bx,%ax
        movl    %edi,%ecx       # Align on 32bits
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%edx
        rep
        stosb
        movl    %edx,%ecx
        andl    $3,%edx
        shrl    $2,%ecx
        rep
        stosl
        movl    %edx,%ecx
.LFill1:
        rep
        stosb
        end;
end;


procedure fillword(var x;count : longint;value : word);

  begin
     asm
        movl 8(%ebp),%edi
        movl 12(%ebp),%ecx
        movl 16(%ebp),%eax
        movl %eax,%edx
        shll $16,%eax
        movw %dx,%ax
        movl %ecx,%edx
        shrl $1,%ecx
        cld
        rep
        stosl
        movl %edx,%ecx
        andl $1,%ecx
        rep
        stosw
     end ['EAX','ECX','EDX','EDI'];
  end;


{$ifndef ordintern}
{!!!!!! not very fast, but easy. }
function ord(b : boolean) : byte;

  begin
     asm
        movb 8(%ebp),%al
        leave
        ret $2
     end;
  end;
{$endif}

function abs(l : longint) : longint;

  begin
     asm
        movl 8(%ebp),%eax
        orl %eax,%eax
        jns .LMABS1
        negl %eax
        .LMABS1:
        leave
        ret $4
     end ['EAX'];
  end;


function odd(l : longint) : boolean;

  begin
    asm
       movl 8(%ebp),%eax
       andl $1,%eax
       setnz %al
       leave
       ret $4
    end ['EAX'];
  end;

function sqr(l : longint) : longint;

  begin
     asm
        movl 8(%ebp),%eax
        imull %eax,%eax
        leave
        ret $4
     end ['EAX'];
  end;

{$ifndef str_intern }
    procedure str(l : longint;var s : string);
{$else str_intern }
    procedure int_str(l : longint;var s : string);
{$endif str_intern }

      var
         buffer : array[0..11] of byte;

      begin
         { Workaround: }
         if l=$80000000 then
           begin
              s:='-2147483648';
              exit;
           end;
         asm
            movl 8(%ebp),%eax    // load Integer
            movl 12(%ebp),%edi      // Load String address
            xorl %ecx,%ecx    // String length=0
            xorl %ebx,%ebx    // Buffer length=0
            movl $0x0a,%esi      // load 10 as dividing constant.
            or %eax,%eax        // Sign ?
            jns .LM2
            neg %eax
            movb $0x2d,1(%edi)   // put '-' in String
            incl %ecx
         .LM2:
            cdq
            idivl %esi,%eax
            addb $0x30,%dl    // convert Rest to ASCII.
            movb %dl,-12(%ebp,%ebx)
            incl %ebx
            cmpl $0,%eax
            jnz .LM2
                        // copy String
         .LM3:
            movb -13(%ebp,%ebx),%al    // -13 because EBX is decreased only
                                       // later.
            movb %al,1(%edi,%ecx)
            incl %ecx
            decl %ebx
            jnz .LM3
            movb %cl,(%edi)      // Copy String length
         end;
      end;

{$ifndef str_intern }
    procedure str(c : cardinal;var s : string);
{$else str_intern }
    procedure int_str(c : cardinal;var s : string);
{$endif str_intern }

      var
         buffer : array[0..14] of byte;

      begin
         asm
            movl 8(%ebp),%eax       // load CARDINAL
            movl 12(%ebp),%edi      // Load String address
            xorl %ecx,%ecx          // String length=0
            xorl %ebx,%ebx          // Buffer length=0
            movl $0x0a,%esi         // load 10 as dividing constant.
         .LM4:
            xorl %edx,%edx
            divl %esi,%eax
            addb $0x30,%dl          // convert Rest to ASCII.
            movb %dl,-12(%ebp,%ebx)
            incl %ebx
            cmpl $0,%eax
            jnz .LM4
            { now copy the string }
         .LM5:
            movb -13(%ebp,%ebx),%al    // -13 because EBX is decreased only
                                       // later.
            movb %al,1(%edi,%ecx)
            incl %ecx
            decl %ebx
            jnz .LM5
            movb %cl,(%edi)            // Copy String length
         end;
      end;

    procedure f1;[public,alias: 'FLUSH_STDOUT'];

      begin
         asm
            pushal
         end;
         FileFunc(textrec(output).flushfunc)(textrec(output));
         asm
            popal
         end;
      end;

Function Sptr : Longint;
begin
  asm
    movl %esp,%eax
    addl $8,%eax
    movl %eax,-4(%ebp)
  end ['EAX'];
end;

{
  $Log$
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.30  1998/03/20 05:11:17  carl
    * bugfix of register usage list for strcmp and strconcat

  Revision 1.29  1998/03/15 19:38:41  peter
    * fixed a bug in Move()

  Revision 1.28  1998/03/10 23:50:39  florian
    * strcopy saves now the used registers except ESI and EDI, solves
      a problem with the optimizer

  Revision 1.27  1998/03/10 16:25:52  jonas
    * removed reloading of eax with 8(ebp), in int_help_constructor, as eax is nowhere modified

  Revision 1.25  1998/03/02 11:44:43  florian
    * writing of large cardinals fixed

  Revision 1.24  1998/03/02 04:14:02  carl
    * page fault bug fix with CHECK_OBJECT
       warning: Will only work with GAS as VMT pointer field is an
       .lcomm and will be ZEROED by linker (might not be true for TASM)

  Revision 1.23  1998/02/24 17:50:46  peter
    * upto 100% (255's char is different ;) faster STRCMP
    * faster StrPas from i386.inc also strings.pp

  Revision 1.22  1998/02/22 22:01:26  carl
  + IOCHECK halts with the correct errorcode now

  Revision 1.21  1998/02/11 16:55:14  michael
  fixed cardinal printing. Large cardinals (>0fffffff) not yet working

  Revision 1.20  1998/02/06 09:12:39  florian
    * bug in CHECK_OBJECT fixed

  Revision 1.19  1998/02/05 22:30:25  florian
    + CHECK_OBJECT to check for an valid VMT (before calling a virtual method)

  Revision 1.18  1998/02/04 14:46:36  daniel
  * Some small tweaks

  Revision 1.17  1998/01/27 22:05:07  florian
    * again small fixes to DOM (Delphi Object Model)

  Revision 1.16  1998/01/26 11:59:01  michael
  + Added log at the end

  revision 1.15
  date: 1998/01/25 22:52:52;  author: peter;  state: Exp;  lines: +140 -122
    * Faster string functions by using aligning
  ----------------------------
  revision 1.14
  date: 1998/01/25 22:30:48;  author: florian;  state: Exp;  lines: +14 -2
    * DOM: some fixes to tobject and the con-/destructor help routines
  ----------------------------
  revision 1.13
  date: 1998/01/23 18:08:29;  author: florian;  state: Exp;  lines: +10 -4
    * more bugs in FCL object model removed
  ----------------------------
  revision 1.12
  date: 1998/01/23 15:54:47;  author: florian;  state: Exp;  lines: +5 -5
    + small extensions to FCL object model
  ----------------------------
  revision 1.11
  date: 1998/01/20 00:14:24;  author: peter;  state: Exp;  lines: +18 -5
    * .type is linux only, go32v2 doesn't like it
  ----------------------------
  revision 1.10
  date: 1998/01/19 16:19:53;  author: peter;  state: Exp;  lines: +7 -1
  * Works now correct with shared libs, .globl always needs a .type
  ----------------------------
  revision 1.9
  date: 1998/01/19 10:21:35;  author: michael;  state: Exp;  lines: +1 -6
  * moved Fillchar t(..,char) to system.inc
  ----------------------------
  revision 1.8
  date: 1998/01/19 09:15:05;  author: michael;  state: Exp;  lines: +40 -132
  * Bugfixes in Move and FillChar
  ----------------------------
  revision 1.7
  date: 1998/01/16 23:10:52;  author: florian;  state: Exp;  lines: +23 -1
    + some tobject stuff
  ----------------------------
  revision 1.6
  date: 1998/01/16 22:21:35;  author: michael;  state: Exp;  lines: +601 -493
  + Installed pentium-optimized move (optional)
  ----------------------------
  revision 1.5
  date: 1998/01/12 03:39:17;  author: carl;  state: Exp;  lines: +2 -2
    * bugfix of RE_OVERFLOW, gives out now a Runerror(215)
  ----------------------------
  revision 1.4
  date: 1998/01/01 16:57:36;  author: michael;  state: Exp;  lines: +1 -21
  Moved DO_EXIT to system.inc. Now processor independent
  ----------------------------
  revision 1.3
  date: 1997/12/10 12:12:31;  author: michael;  state: Exp;  lines: +2 -2
  * changed dateifunc to FileFunc
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:34:36;  author: michael;  state: Exp;  lines: +13 -0
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:48;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:48;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
