{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Pierre Muller,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

var argc : longint;
    doscmd : string;
    args : ^pchar;

function far_strlen(selector : word;linear_address : longint) : longint;
begin
asm
	movl linear_address,%edx
	movl %edx,%ecx
	movw selector,%gs
Larg19:
	movb %gs:(%edx),%al
	testb %al,%al
	je Larg20
	incl %edx
	jmp Larg19
Larg20:
	movl %edx,%eax
	subl %ecx,%eax
        movl %eax,__RESULT
end;
end;

function atohex(s : pchar) : longint;
var rv : longint;
    v : byte;
begin
rv := 0;
while (s^ <>#0) do
  begin
  v := ord(s^) - ord('0');
  if (v > 9) then v := v - 7;
  v := v and 15; { in case it's lower case }
  rv := rv*16 + v;
  inc(longint(s));
  end;
atohex := rv;
end;

procedure setup_arguments;
type  arrayword = array [0..0] of word;
var psp : word;
    i,j : byte;
    quote : char;
    proxy_s : string[7];
    al,proxy_argc,proxy_seg,proxy_ofs,lin : longint;
    largs : array[0..127] of pchar;
    rm_argv : ^arrayword;
begin
for i := 1 to 127  do
   largs[i] := nil;
psp:=stub_info^.psp_selector;
largs[0]:=dos_argv0;
argc := 1;
sysseg_move(psp, 128, get_ds, longint(@doscmd), 128);
{$IfDef SYSTEMDEBUG}
Writeln('Dos command line is #',doscmd,'# size = ',length(doscmd));
{$EndIf SYSTEMDEBUG}
j := 1;
quote := #0;
for i:=1 to length(doscmd) do
  Begin
  if doscmd[i] = quote then
    begin
    quote := #0;
    doscmd[i] := #0;
    largs[argc]:=@doscmd[j];
    inc(argc);
    j := i+1;
    end else
  if (quote = #0) and ((doscmd[i] = '''') or (doscmd[i]='"')) then
    begin
    quote := doscmd[i];
    j := i + 1;
    end else
  if (quote = #0) and ((doscmd[i] = ' ')
    or (doscmd[i] = #9) or (doscmd[i] = #10) or
    (doscmd[i] = #12) or (doscmd[i] = #9)) then
    begin
    doscmd[i]:=#0;
    if j<i then
      begin
      largs[argc]:=@doscmd[j];
      inc(argc);
      j := i+1;
      end else inc(j);
    end else
  if (i = length(doscmd)) then
    begin
    doscmd[i+1]:=#0;
    largs[argc]:=@doscmd[j];
    inc(argc);
    end;
  end;

if (argc > 1) and (far_strlen(get_ds,longint(largs[1])) = 6)  then
  begin
  move(largs[1]^,proxy_s[1],6);
  proxy_s[0] := #6;
  if (proxy_s = '!proxy') then
    begin
{$IfDef SYSTEMDEBUG}
    Writeln('proxy command line ');
{$EndIf SYSTEMDEBUG}
    proxy_argc := atohex(largs[2]);
    proxy_seg  := atohex(largs[3]);
    proxy_ofs := atohex(largs[4]);
    getmem(rm_argv,proxy_argc*sizeof(word));
    sysseg_move(dos_selector,proxy_seg*16+proxy_ofs, get_ds,longint(rm_argv),proxy_argc*sizeof(word));
    for i:=0 to proxy_argc - 1 do
      begin
      lin := proxy_seg*16 + rm_argv^[i];
      al :=far_strlen(dos_selector, lin);
      getmem(largs[i],al+1);
      sysseg_move(dos_selector, lin, get_ds,longint(largs[i]), al+1);
{$IfDef SYSTEMDEBUG}
      Writeln('arg ',i,' #',largs[i],'#');
{$EndIf SYSTEMDEBUG}
      end;
    argc := proxy_argc;
    end;
  end;
getmem(args,argc*SizeOf(pchar));
for i := 0 to argc-1  do
   args[i] := largs[i];
end;

function strcopy(dest,source : pchar) : pchar;
    
      begin
         asm
            cld
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            xorb %al,%al
            repne
            scasb
            not %ecx
            movl 8(%ebp),%edi
            movl 12(%ebp),%esi
            movl %ecx,%eax
            shrl $2,%ecx
            rep
            movsl
            movl %eax,%ecx
            andl $3,%ecx
            rep
            movsb
            movl 8(%ebp),%eax
            leave
            ret $8
         end;
      end;


procedure setup_environment;
var env_selector : word;
    env_count : longint;
    dos_env,cp : pchar;
    stubaddr : p_stub_info;
begin
   asm
   movl __stubinfo,%eax
   movl %eax,stubaddr
   end;
   stub_info:=stubaddr;
   getmem(dos_env,stub_info^.env_size);
   env_count:=0;
   sysseg_move(stub_info^.psp_selector,$2c, get_ds, longint(@env_selector), 2);
   sysseg_move(env_selector, 0, get_ds, longint(dos_env), stub_info^.env_size);
  cp:=dos_env;
  while cp ^ <> #0 do
    begin
    inc(env_count);
    while (cp^ <> #0) do inc(longint(cp)); { skip to NUL }
    inc(longint(cp)); { skip to next character }
    end;
  getmem(environ,(env_count+1) * sizeof(pchar));
  if (environ = nil) then exit;
  cp:=dos_env;
  env_count:=0;
  while cp^ <> #0 do
    begin
    getmem(environ[env_count],strlen(cp)+1);
    strcopy(environ[env_count], cp);
{$IfDef SYSTEMDEBUG}
      Writeln('env ',env_count,' = "',environ[env_count],'"');
{$EndIf SYSTEMDEBUG}
    inc(env_count);
    while (cp^ <> #0) do inc(longint(cp)); { skip to NUL }
    inc(longint(cp)); { skip to next character }
    end;
  environ[env_count]:=nil;
  inc(longint(cp),3);
  getmem(dos_argv0,strlen(cp)+1);
  if (dos_argv0 = nil) then halt;
  strcopy(dos_argv0, cp);
end;

{
  $Log$
  Revision 1.1  1998-03-25 11:18:42  root
  Initial revision

  Revision 1.3  1998/01/26 11:57:15  michael
  + Added log at the end


  
  Working file: rtl/dos/go32v2/sargs.inc
  description:
  ----------------------------
  revision 1.2
  date: 1997/12/01 15:35:01;  author: michael;  state: Exp;  lines: +14 -0
  + Added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:52;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:52;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
