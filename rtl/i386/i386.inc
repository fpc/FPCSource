{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    intel i386+


    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                                Move / Fill
****************************************************************************}

{$I386_ATT}

procedure Move(var source;var dest;count:longint);assembler;

asm
    movl    dest,%edi
    movl    source,%esi
    movl    %edi,%eax
    movl    count,%ebx
    {Check for back or forward}
    sub     %esi,%eax
    jz      .LMoveEnd       {Do nothing when source=dest}
    jc      .LFMove         {Do forward, dest<source}
    cmp     %ebx,%eax
    jb      .LBMove         {Dest is in range of move, do backward}
    {Forward Copy}
.LFMove:
    cld
    cmpl    $7,%ebx
    jl      .LFMove1
    movl    %edi,%ecx       {Align on 32bits}
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
    jmp     .LMoveEnd
    {Backward Copy}
.LBMove:
    std
    addl    %ebx,%esi
    addl    %ebx,%edi
    movl    %edi,%ecx
    decl    %esi
    decl    %edi
    cmpl    $7,%ebx
    jl      .LBMove1
    negl    %ecx            {Align on 32bits}
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
    movl    %ebx,%ecx
    addl    $3,%esi
    addl    $3,%edi
.LBMove1:
    rep
    movsb
    cld
.LMoveEnd:
end;

Procedure FillChar(var x;count:longint;value:byte);[alias: 'FILL_OBJECT'];assembler;
asm
    cld
    movl    x,%edi
    movl    value,%eax
    movl    count,%ecx
    cmpl    $7,%ecx
    jl      .LFill1
    movl    %ecx,%edx
    movb    %al,%ah
    movl    %eax,%ebx
    shll    $16,%eax
    movw    %bx,%ax
    movl    %edi,%ecx
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

{$ifndef RTLLITE}
procedure fillword(var x;count : longint;value : word);assembler;
asm
    movl    x,%edi
    movl    count,%ecx
    movl    value,%eax
    movl    %eax,%ebx
    shll    $16,%eax
    movw    %bx,%ax
    movl    %ecx,%edx
    shrl    $1,%ecx
    cld
    rep
    stosl
    {Ecx is zero.}
    movb    %dl,%cl
    andb    $1,%cl         {Saves some bytes, no speed penalties.}
    rep
    stosw
end ['EAX','ECX','EDX','EDI'];
{$endif RTLLITE}


{****************************************************************************
                              Object Helpers
****************************************************************************}

{$I386_DIRECT}

procedure int_help_constructor;assembler; [public,alias:'HELP_CONSTRUCTOR'];
asm
{ Entry without preamble, since we need the ESP of the constructor
  Stack (relative to %ebp):
    12 Self
     8 VMT-Address
     4 main programm-Addr
     0 %ebp
}
      { eax isn't touched anywhere, so it doesn't have to reloaded }
        movl    8(%ebp),%eax
      { initialise self ? }
        orl     %esi,%esi
        jne     .LHC_4
      { get memory, but save register first temporary variable }
        subl    $4,%esp
        movl    %esp,%esi
      { Save Register}
        pushal
      { Memory size }
        pushl   (%eax)
        pushl   %esi
        call    GETMEM
        popal
      { Memory size to %esi }
        movl    (%esi),%esi
        addl    $4,%esp
      { If no memory available : fail() }
        orl     %esi,%esi
        jz      .LHC_5
      { init self for the constructor }
        movl    %esi,12(%ebp)
.LHC_4:
      { is there a VMT address ? }
        orl     %eax,%eax
        jnz     .LHC_7
      { In case the constructor doesn't do anything, the Zero-Flag }
      { can't be put, because this calls Fail() }
        incl    %eax
        ret
.LHC_7:
      { set zero inside the object }
        pushal
        pushw   $0
        pushl   (%eax)
        pushl   %esi
        call    FILL_OBJECT
        popal
      { set the VMT address for the new created object }
        movl    %eax,(%esi)
        orl     %eax,%eax
.LHC_5:
end;


procedure help_fail;assembler;
asm
end;


procedure int_new_class;assembler;[public,alias:'NEW_CLASS'];
asm
     { create class ? }
     movl 8(%ebp),%edi
     orl %edi,%edi
     jz .LNEW_CLASS1
     { esi contains the vmt }
     pushl %esi
     { call newinstance (class method!) }
     call 16(%esi)
     { newinstance returns a pointer to the new created }
     { instance in eax                                  }
     { load esi and insert self                         }
     movl %eax,8(%ebp)
     movl %eax,%esi
     ret
.LNEW_CLASS1:
     movl %esi,8(%ebp)
end;


procedure int_dispose_class;assembler;[public,alias:'DISPOSE_CLASS'];
asm
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
end;


{ checks for a correct vmt pointer }
procedure int_check_object;assembler;[public,alias:'CHECK_OBJECT'];
asm
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


procedure int_help_destructor;assembler;[public,alias:'HELP_DESTRUCTOR'];
asm
{ Stack (relative to %ebp):
    12 Self
     8 VMT-Address
     4 Main program-Addr
     0 %ebp
}
      { temporary Variable }
        subl    $4,%esp
        movl    %esp,%edi
        pushal
      { Should the object be resolved ? }
        movl    8(%ebp),%eax
        orl     %eax,%eax
        jz      .LHD_3
      { Yes, get size from SELF! }
        movl    12(%ebp),%eax
      { get VMT-pointer (from Self) to %ebx }
        movl    (%eax),%ebx
      { And put size on the Stack }
        pushl   (%ebx)
      { SELF }
      { I think for precaution }
      { that we should clear the VMT here }
        movl    $0,(%eax)
        movl    %eax,(%edi)
        pushl   %edi
        call    FREEMEM
.LHD_3:
        popal
        addl    $4,%esp
end;


{****************************************************************************
                                 String
****************************************************************************}

{$I386_ATT}

procedure int_strcopy(len:longint;sstr,dstr:pointer);[public,alias:'STRCOPY'];
{
  this procedure must save all modified registers except EDI and ESI !!!
}
begin
  asm
        pushl   %eax
        pushl   %ecx
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
        movl    %edi,%ecx       { Align on 32bits }
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
        popl    %ecx
        popl    %eax
  end ['ECX','EAX','ESI','EDI'];
end;


procedure int_strconcat(s1,s2 : pointer);[public,alias: 'STRCONCAT'];
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
        movl    %edi,%ecx       { Align on 32bits }
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


procedure int_strcmp(dstr,sstr : pointer);[public,alias: 'STRCMP'];
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
        movl    %edi,%ecx       { Align on 32bits }
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

{****************************************************************************
                                  PChar
****************************************************************************}

function strpas(p:pchar):string;
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

        movl    __RESULT,%edi
        notb    %al
        decl    %eax
        stosb
        cmpl    $7,%eax
        jl      .LStrPas2
        movl    %edi,%ecx       { Align on 32bits }
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


function strlen(p:pchar):longint;assembler;
asm
        movl    p,%edi
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        cld
        repne
        scasb
        movl    $0xfffffffe,%eax
        subl    %ecx,%eax
end ['EDI','ECX','EAX'];


{****************************************************************************
                                 Other

****************************************************************************}

Function Sptr : Longint;assembler;
asm
        movl    %esp,%eax
        addl    $4,%eax
end ['EAX'];


function get_addr(addrbp:longint):longint;assembler;
asm
        movl    addrbp,%eax
        orl     %eax,%eax
        jz      .Lg_a_null
        movl    4(%eax),%eax
.Lg_a_null:
end ['EAX'];


function get_next_frame(framebp:longint):longint;assembler;
asm
        movl    framebp,%eax
        orl     %eax,%eax
        jz      .Lgnf_null
        movl    (%eax),%eax
.Lgnf_null:
end ['EAX'];


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



procedure int_iocheck(addr : longint);[public,alias: 'IOCHECK'];
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


procedure int_re_overflow;[public,alias: 'RE_OVERFLOW'];
var
  addr : longint;
begin
{ Overflow was shortly before the return address }
   asm
        movl    4(%ebp),%edi
        movl    %edi,addr
   end;
   writeln('Overflow at ',addr);
   RunError(215);
end;


procedure int_flush_stdout;[public,alias: 'FLUSH_STDOUT'];
begin
  asm
        pushal
  end;
  FileFunc(textrec(output).flushfunc)(textrec(output));
  asm
        popal
  end;
end;



{****************************************************************************
                                 Math Functions
****************************************************************************}

function abs(l:longint):longint;assembler;
asm
        movl    l,%eax
        orl     %eax,%eax
        jns     .LMABS1
        negl    %eax
.LMABS1:
end ['EAX'];


function odd(l:longint):boolean;assembler;
asm
       movl     l,%eax
       andl     $1,%eax
       setnz    %al
end ['EAX'];


function sqr(l:longint):longint;assembler;
asm
        mov     l,%eax
        imull   %eax,%eax
end ['EAX'];


procedure int_str(l : longint;var s : string);
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
        movl    8(%ebp),%eax            // load Integer
        movl    12(%ebp),%edi           // Load String address
        xorl    %ecx,%ecx               // String length=0
        xorl    %ebx,%ebx               // Buffer length=0
        movl    $0x0a,%esi              // load 10 as dividing constant.
        or      %eax,%eax               // Sign ?
        jns     .LM2
        neg     %eax
        movb    $0x2d,1(%edi)           // put '-' in String
        incl    %ecx
.LM2:
        cdq
        idivl   %esi,%eax
        addb    $0x30,%dl               // convert Rest to ASCII.
        movb    %dl,-12(%ebp,%ebx)
        incl    %ebx
        cmpl    $0,%eax
        jnz     .LM2
.LM3:
        movb    -13(%ebp,%ebx),%al      // -13 because EBX is decreased only later
        movb    %al,1(%edi,%ecx)
        incl    %ecx
        decl    %ebx
        jnz     .LM3
        movb    %cl,(%edi)              // Copy String length
  end;
end;


procedure int_str(c : cardinal;var s : string);
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
.LM5:                           // now copy the string
        movb -13(%ebp,%ebx),%al    // -13 because EBX is decreased only later
        movb %al,1(%edi,%ecx)
        incl %ecx
        decl %ebx
        jnz .LM5
        movb %cl,(%edi)            // Copy String length
  end;
end;

Function Random(L: LongInt): LongInt;assembler;
asm
          movl  $134775813,%eax
          mull  RandSeed
          incl  %eax
          movl  %eax,RandSeed
          mull  4(%esp)
          movl  %edx,%eax
end;

{$I386_DIRECT}

{
  $Log$
  Revision 1.9  1998-05-22 12:34:07  peter
    * fixed the optimizes of daniel

  Revision 1.8  1998/05/20 11:01:52  peter
    * .FILL_OBJECT and FILL_OBJECT are not the same names ;)

  Revision 1.7  1998/05/20 08:09:24  daniel
  * Some extra use of ;assembler;.

  Revision 1.6  1998/05/12 10:42:41  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

  Revision 1.5  1998/04/29 13:28:19  peter
    * some cleanup and i386_att usage

  Revision 1.4  1998/04/10 15:41:54  florian
    + some small comments added

  Revision 1.3  1998/04/10 15:25:23  michael
  - Removed so-called better random function

  Revision 1.2  1998/04/08 07:53:31  michael
  + Changed Random() function. Moved from system to processor dependent files (from Pedro Gimeno)
}
