{
    Copyright (c) 1998-2013 by the FPC Development Team

    Dumps the contents of a FPC unit file (PPU File)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}
program ppudump;

{$i fpcdefs.inc}
{$H+}

{$define IN_PPUDUMP}
uses
  { do NOT add symconst or globtype to make merging easier }
  { do include symconst and globtype now before splitting 2.5 PM 2011-06-15 }
  SysUtils,
  constexp,
  symconst,
  ppu,
  systems,
  globals,
  globtype,
  widestr,
  tokens,
  version,
  ppuout,
  ppujson,
  ppuxml;

const
  Title     = 'PPU-Analyser';
  Copyright = 'Copyright (c) 1998-2013 by the Free Pascal Development Team';

{ verbosity }
  v_none           = $0;
  v_header         = $1;
  v_defs           = $2;
  v_syms           = $4;
  v_interface      = $8;
  v_implementation = $10;
//  v_browser        = $20;
  v_all            = $ff;

{ not needed anymore $i systems.inc }

{ List of all supported cpus }
const
  CpuTxt : array[tsystemcpu] of string[9]=
    (
    {  0 } 'none',
    {  1 } 'i386',
    {  2 } 'm68k',
    {  3 } 'alpha',
    {  4 } 'powerpc',
    {  5 } 'sparc',
    {  6 } 'vis',
    {  7 } 'ia64',
    {  8 } 'x86_64',
    {  9 } 'mipseb',
    { 10 } 'arm',
    { 11 } 'powerpc64',
    { 12 } 'avr',
    { 13 } 'mipsel',
    { 14 } 'jvm',
    { 15 } 'i8086'
    );

{ List of all supported system-cpu couples }
const
  Targets : array[tsystem] of string[18]=(
  { 0 }   'none',
  { 1 }   'GO32V1 (obsolete)',
  { 2 }   'GO32V2',
  { 3 }   'Linux-i386',
  { 4 }   'OS/2',
  { 5 }   'Win32',
  { 6 }   'FreeBSD-i386',
  { 7 }   'Amiga',
  { 8 }   'Atari',
  { 9 }   'MacOS-m68k',
  { 10 }  'Linux-m68k',
  { 11 }  'PalmOS-m68k',
  { 12 }  'Linux-alpha',
  { 13 }  'Linux-ppc',
  { 14 }  'MacOS-ppc',
  { 15 }  'Solaris-i386',
  { 16 }  'BeOS-i386',
  { 17 }  'NetBSD-i386',
  { 18 }  'NetBSD-m68k',
  { 19 }  'Netware-i386-clib',
  { 20 }  'Qnx-i386',
  { 21 }  'WDOSX-i386',
  { 22 }  'Solaris-sparc',
  { 23 }  'Linux-sparc',
  { 24 }  'OpenBSD-i386',
  { 25 }  'OpenBSD-m68k',
  { 26 }  'Linux-x86-64',
  { 27 }  'MacOSX-ppc',
  { 28 }  'OS/2 via EMX',
  { 29 }  'NetBSD-powerpc',
  { 30 }  'OpenBSD-powerpc',
  { 31 }  'Linux-arm',
  { 32 }  'Watcom-i386',
  { 33 }  'MorphOS-powerpc',
  { 34 }  'FreeBSD-x86-64',
  { 35 }  'Netware-i386-libc',
  { 36 }  'Amiga-PowerPC',
  { 37 }  'Win64-x64',
  { 38 }  'WinCE-ARM',
  { 39 }  'Win64-iA64',
  { 40 }  'WinCE-i386',
  { 41 }  'Linux-x64',
  { 42 }  'GBA-arm',
  { 43 }  'Linux-powerpc64',
  { 44 }  'Darwin-i386',
  { 45 }  'PalmOS-arm',
  { 46 }  'MacOSX-powerpc64',
  { 47 }  'NDS-arm',
  { 48 }  'Embedded-i386',
  { 49 }  'Embedded-m68k',
  { 50 }  'Embedded-alpha',
  { 51 }  'Embedded-powerpc',
  { 52 }  'Embedded-sparc',
  { 53 }  'Embedded-vm',
  { 54 }  'Embedded-iA64',
  { 55 }  'Embedded-x64',
  { 56 }  'Embedded-mips',
  { 57 }  'Embedded-arm',
  { 58 }  'Embedded-powerpc64',
  { 59 }  'Symbian-i386',
  { 60 }  'Symbian-arm',
  { 61 }  'MacOSX-x64',
  { 62 }  'Embedded-avr',
  { 63 }  'Haiku-i386',
  { 64 }  'Darwin-ARM',
  { 65 }  'Solaris-x86-64',
  { 66 }  'Linux-MIPS',
  { 67 }  'Linux-MIPSel',
  { 68 }  'NativeNT-i386',
  { 69 }  'iPhoneSim-i386',
  { 70 }  'Wii-powerpc',
  { 71 }  'OpenBSD-x86-64',
  { 72 }  'NetBSD-x86-64',
  { 73 }  'AIX-powerpc',
  { 74 }  'AIX-powerpc64',
  { 75 }  'Java-JVM',
  { 76 }  'Android-JVM',
  { 77 }  'Android-arm',
  { 78 }  'Android-i386',
  { 79 }  'MSDOS-i8086'
  );

const
{ in widestr, we have the following definition
  type
       tcompilerwidechar = word;
  thus widecharsize seems to always be 2 bytes }

  widecharsize : longint = 2;
  cpu : tsystemcpu = cpu_no;

{ This type is defined in scanner.pas unit }
type
  tspecialgenerictoken = (
    ST_LOADSETTINGS,
    ST_LINE,
    ST_COLUMN,
    ST_FILEINDEX,
    ST_LOADMESSAGES);


var
  ppufile     : tppufile;
  ppuversion  : dword;
  space       : string;
  verbose     : longint;
  derefdata   : pbyte;
  derefdatalen : longint;
  pout: TPpuOutput;
  nostdout: boolean;
  UnitList: TPpuContainerDef;
  CurUnit: TPpuUnitDef;
  SkipVersionCheck: boolean;


{****************************************************************************
                          Helper Routines
****************************************************************************}

{****************************************************************************
                          Routine to read 80-bit reals
****************************************************************************
}
type
  TSplit80bitReal = packed record
    case byte of
      0: (bytes: Array[0..9] of byte);
      1: (words: Array[0..4] of word);
      2: (cards: Array[0..1] of cardinal; w: word);
  end;
const
  maxDigits = 17;
  function Real80bitToStr(var e : TSplit80bitReal) : string;
  var
    Temp : string;
    new : TSplit80bitReal;
    fraczero, expmaximal, sign, outside_double : boolean;
    exp : smallint;
    ext : extended;
    d : double;
    i : longint;
    mantval : qword;
  begin
    if ppufile.change_endian then
      begin
        for i:=0 to 9 do
          new.bytes[i]:=e.bytes[9-i];
        e:=new;
      end;
    if sizeof(ext)=10 then
      begin
        ext:=pextended(@e)^;
        str(ext,result);
        exit;
      end;
    { extended, format (MSB): 1 Sign bit, 15 bit exponent, 64 bit mantissa }
    sign := (e.w and $8000) <> 0;
    expMaximal := (e.w and $7fff) = 32767;
    exp:=(e.w and $7fff) - 16383 - 63;
    fraczero := (e.cards[0] = 0) and
                    ((e.cards[1] and $7fffffff) = 0);
    mantval := qword(e.cards[0]) or (qword(e.cards[1]) shl 32);
    if expMaximal then
      if fraczero then
        if sign then
          temp := '-Inf'
        else temp := '+Inf'
      else temp := 'Nan'
    else
      begin
        d:=double(mantval);
        if sign then
          d:=-d;
        outside_double:=false;
        Try
          if exp > 0 then
            begin
              for i:=1 to exp do
                d:=d *2.0;
            end
          else if exp < 0 then
            begin
              for i:=1 to -exp do
                d:=d /2.0;
            end;
        Except
          outside_double:=true;
        end;
      if (mantval<>0) and (d=0.0) then
        outside_double:=true;
      if outside_double then
        Temp:='Extended value outside double bound'
      else
        system.str(d,temp);

      end;

    result:=temp;
  end;

const has_errors : boolean = false;
      has_more_infos : boolean = false;

procedure Write(const s: string);
begin
  if nostdout then exit;
  system.write(s);
end;

procedure Write(const params: array of const);
var
  i: integer;
begin
  if nostdout then exit;
  for i:=Low(params) to High(params) do
    with TVarRec(params[i]) do
      case VType of
        vtInteger: system.write(VInteger);
        vtInt64: system.write(VInt64^);
        vtQWord: system.write(VQWord^);
        vtString: system.write(VString^);
        vtAnsiString: system.write(ansistring(VAnsiString));
        vtPChar: system.write(VPChar);
        vtChar: system.write(VChar);
        vtBoolean: system.write(VBoolean);
        vtExtended: system.write(VExtended^);
        else
          begin
            system.writeln;
            system.writeln('Unsupported var type: ', VType);
            Halt(10);
          end;
      end;
end;

procedure Writeln(const s: string = '');
begin
  if nostdout then exit;
  system.writeln(s);
end;

procedure Writeln(const params: array of const);
begin
  if nostdout then exit;
  Write(params);
  system.writeln;
end;

Procedure HasMoreInfos;
begin
  Writeln('!! Entry has more information stored');
  has_more_infos:=true;
end;

procedure SetHasErrors;
begin
  has_errors:=true;
end;

Procedure WriteError(const S : string);
Begin
  system.Writeln(StdErr, S);
  SetHasErrors;
End;

Procedure WriteWarning(const S : string);
var
  ss: string;
Begin
  ss:='!! Warning: ' + S;
  if nostdout then
    system.Writeln(StdErr, ss)
  else
    system.Writeln(ss);
End;

function Unknown(const st : string; val :longint) : string;
Begin
  Unknown:='<!! Unknown'+st+' value '+tostr(val)+'>';
  SetHasErrors;
end;

function ToStr(w:longint):String;
begin
  Str(w,ToStr);
end;

Function Target2Str(w:longint):string;
begin
  if w<=ord(high(tsystem)) then
    Target2Str:=Targets[tsystem(w)]
  else
    Target2Str:=Unknown('target',w);
end;


Function Cpu2Str(w:longint):string;
begin
  if w<=ord(high(tsystemcpu)) then
    begin
      cpu:=tsystemcpu(w);
      Cpu2Str:=CpuTxt[cpu];
    end
  else
    Cpu2Str:=Unknown('cpu',w);
end;


Function Varspez2Str(w:longint):string;
const
  { in symconst unit
    tvarspez = (vs_value,vs_const,vs_var,vs_out,vs_constref); }
  varspezstr : array[tvarspez] of string[8]=('Value','Const','Var','Out','ConstRef','Final');
begin
  if w<=ord(high(varspezstr)) then
    Varspez2Str:=varspezstr[tvarspez(w)]
  else
    Varspez2Str:=Unknown('varspez',w);
end;

Function VarRegable2Str(w:longint):string;
  { tvarregable type is defined in symconst unit }
const
  varregableStr : array[tvarregable] of string[6]=('None','IntReg','FPUReg','MMReg','Addr');
begin
  if w<=ord(high(varregablestr)) then
    Varregable2Str:=varregablestr[tvarregable(w)]
  else
    Varregable2Str:=Unknown('regable',w);
end;


Function Visibility2Str(w:longint):string;
const
  { tvisibility type is defined in symconst unit }

  visibilityName : array[tvisibility] of string[16] = (
    'hidden','strict private','private','strict protected','protected',
    'public','published','<none>'
  );
begin
  if w<=ord(high(visibilityName)) then
    result:=visibilityName[tvisibility(w)]
  else
    result:=Unknown('visibility',w);
end;


Function IntfEntryType2Str(w:longint):string;
const
  { tinterfaceentrytype type is defined in symconst unit }

  Name : array[tinterfaceentrytype] of string = (
    'standard','virtual method result','static method result','field value','virtual method class',
    'static method class','field value class'
  );
begin
  if w<=ord(high(Name)) then
    result:=Name[tinterfaceentrytype(w)]
  else
    result:=Unknown('entry type',w);
end;


Function Synthetic2Str(w: byte): string;
const
   syntheticName : array[tsynthetickind] of string[length('jvm procvar intf constructor')] = (
      '<none>','anon inherited','jvm clone','record deep copy',
      'record initilializer', 'empty routine', 'typed const initializer',
      'callthough', 'callthrough if not abstract', 'jvm enum values',
      'jvm enum valueof', 'jvm enum class constructor',
      'jvm enum jumps constructor', 'jvm enum fpcordinal',
      'jvm enum fpcvalueof', 'jvm enum long2set',
      'jvm enum bitset2set', 'jvm enum set2set',
      'jvm procvar invoke', 'jvm procvar intf constructor',
      'jvm virtual class method', 'jvm field getter', 'jvm field setter');
begin
  if w<=ord(high(syntheticName)) then
    result:=syntheticName[tsynthetickind(w)]
  else
    result:=Unknown('synthetickind',w);
end;

function PPUFlags2Str(flags:longint):string;
type
  tflagopt=record
    mask : longint;
    str  : string[30];
  end;
const
  flagopts=27;
  flagopt : array[1..flagopts] of tflagopt=(
    (mask: $1    ;str:'init'),
    (mask: $2    ;str:'final'),
    (mask: $4    ;str:'big_endian'),
    (mask: $8    ;str:'dbx'),
//    (mask: $10   ;str:'browser'),
    (mask: $20   ;str:'in_library'),
    (mask: $40   ;str:'smart_linked'),
    (mask: $80   ;str:'static_linked'),
    (mask: $100  ;str:'shared_linked'),
//    (mask: $200  ;str:'local_browser'),
    (mask: $400  ;str:'no_link'),
    (mask: $800  ;str:'has_resources'),
    (mask: $1000  ;str:'little_endian'),
    (mask: $2000  ;str:'release'),
    (mask: $4000  ;str:'local_threadvars'),
    (mask: $8000  ;str:'fpu_emulation_on'),
    (mask: $210000  ;str:'has_debug_info'),
    (mask: $10000  ;str:'stabs_debug_info'),
    (mask: $200000  ;str:'dwarf_debug_info'),
    (mask: $20000  ;str:'local_symtable'),
    (mask: $40000  ;str:'uses_variants'),
    (mask: $80000  ;str:'has_resourcefiles'),
    (mask: $100000  ;str:'has_exports'),
    (mask: $400000  ;str:'has_wideinits'),
    (mask: $800000  ;str:'has_classinits'),
    (mask: $1000000 ;str:'has_resstrinits'),
    (mask: $2000000 ;str:'i8086_far_code'),
    (mask: $4000000 ;str:'i8086_far_data'),
    (mask: $8000000 ;str:'i8086_huge_data')
  );
var
  i,ntflags : longint;
  first  : boolean;
  s : string;
begin
  s:='';
  if flags<>0 then
   begin
     ntflags:=flags;
     first:=true;
     for i:=1to flagopts do
      if (flags and flagopt[i].mask)<>0 then
       begin
         if first then
           first:=false
         else
           s:=s+', ';
         s:=s+flagopt[i].str;
         ntflags:=ntflags and (not flagopt[i].mask);
       end;
   end
  else
   s:='none';
  if ntflags<>0 then
    begin
      s:=s+' unknown '+hexstr(ntflags,8);
      SetHasErrors;
    end;
  PPUFlags2Str:=s;
end;


    Function L0(l:longint):string;
    {
      return the string of value l, if l<10 then insert a zero, so
      the string is always at least 2 chars '01','02',etc
    }
      var
        s : string;
      begin
        Str(l,s);
        if l<10 then
         s:='0'+s;
        L0:=s;
      end;


   function  filetimestring( t : longint) : string;
   {
     convert dos datetime t to a string YY/MM/DD HH:MM:SS
   }
     var
       DT : TDateTime;
       hsec : word;
       Year,Month,Day: Word;
       hour,min,sec : word;
     begin
       if t=-1 then
        begin
          Result := 'Not Found';
          SetHasErrors;
          exit;
        end;
       DT := FileDateToDateTime(t);
       DecodeTime(DT,hour,min,sec,hsec);
       DecodeDate(DT,year,month,day);
       Result := L0(Year)+'/'+L0(Month)+'/'+L0(Day)+' '+L0(Hour)+':'+L0(min)+':'+L0(sec);
     end;


{****************************************************************************
                             Read Routines
****************************************************************************}

procedure readrecsymtableoptions;
var
  usefieldalignment : shortint;
begin
  if ppufile.readentry<>ibrecsymtableoptions then
    begin
      SetHasErrors;
      exit;
    end;
  writeln([space,' recordalignment: ',shortint(ppufile.getbyte)]);
  usefieldalignment:=shortint(ppufile.getbyte);
  writeln([space,' usefieldalignment: ',usefieldalignment]);
  if (usefieldalignment=C_alignment) then
    writeln([space,' fieldalignment: ',shortint(ppufile.getbyte)]);
end;

procedure readsymtableoptions(const s: string);
type
  tsymtblopt=record
    mask : tsymtableoption;
    str  : string[30];
  end;
const
  symtblopts=ord(high(tsymtableoption))  + 1;
  symtblopt : array[1..symtblopts] of tsymtblopt=(
     (mask:sto_has_helper;   str:'Has helper'),
     (mask:sto_has_generic;  str:'Has generic'),
     (mask:sto_has_operator; str:'Has operator'),
     (mask:sto_needs_init_final;str:'Needs init final table')
  );
var
  options : tsymtableoptions;
  first : boolean;
  i : integer;
begin
  if ppufile.readentry<>ibsymtableoptions then
    begin
      SetHasErrors;
      exit;
    end;
  ppufile.getsmallset(options);
  if space<>'' then
   writeln([space,'------ ',s,' ------']);
  write([space,'Symtable options: ']);
  if options<>[] then
   begin
     first:=true;
     for i:=1 to symtblopts do
      if (symtblopt[i].mask in options) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(symtblopt[i].str);
       end;
   end
  else
   write('none');
  writeln;
end;

procedure readdefinitions(const s:string; ParentDef: TPpuContainerDef); forward;
procedure readsymbols(const s:string; ParentDef: TPpuContainerDef = nil); forward;

procedure readsymtable(const s: string; ParentDef: TPpuContainerDef = nil);
begin
  readsymtableoptions(s);
  readdefinitions(s, ParentDef);
  readsymbols(s, ParentDef);
end;

Procedure ReadLinkContainer(const prefix:string);
{
  Read a serie of strings and write to the screen starting every line
  with prefix
}
  function maskstr(m:longint):string;
    { link options are in globtype unit
  const
    link_none    = $0;
    link_always  = $1;
    link_static  = $2;
    link_smart   = $4;
    link_shared  = $8; }
  var
    s : string;
  begin
    s:='';
    if (m and link_always)<>0 then
     s:=s+'always ';
    if (m and link_static)<>0 then
     s:=s+'static ';
    if (m and link_smart)<>0 then
     s:=s+'smart ';
    if (m and link_shared)<>0 then
     s:=s+'shared ';
    maskstr:=s;
  end;

var
  s : string;
  m : longint;
begin
  while not ppufile.endofentry do
   begin
     s:=ppufile.getstring;
     m:=ppufile.getlongint;
     WriteLn([prefix,s,' (',maskstr(m),')']);
   end;
end;


Procedure ReadContainer(const prefix:string);
{
  Read a serie of strings and write to the screen starting every line
  with prefix
}
begin
  while not ppufile.endofentry do
   WriteLn([prefix,ppufile.getstring]);
end;


procedure ReadLoadUnit;
var
  ucrc,uintfcrc, indcrc : cardinal;
  un: TPpuUnitDef;
begin
  while not ppufile.EndOfEntry do
    begin
      un:=TPpuUnitDef.Create(CurUnit.UsedUnits);
      un.Name:=ppufile.getstring;
      write(['Uses unit: ',un.Name]);
      ucrc:=cardinal(ppufile.getlongint);
      uintfcrc:=cardinal(ppufile.getlongint);
      indcrc:=cardinal(ppufile.getlongint);
      writeln([' (Crc: ',hexstr(ucrc,8),', IntfcCrc: ',hexstr(uintfcrc,8),', IndCrc: ',hexstr(indcrc,8),')']);
      un.Crc:=ucrc;
      un.IntfCrc:=uintfcrc;
    end;
end;


Procedure ReadDerefmap;
var
  i,mapsize : longint;
  s: string;
begin
  mapsize:=ppufile.getlongint;
  writeln(['DerefMapsize: ',mapsize]);
  SetLength(CurUnit.RefUnits, mapsize);
  for i:=0 to mapsize-1 do
    begin
      s:=ppufile.getstring;
      writeln(['DerefMap[',i,'] = ',s]);
      CurUnit.RefUnits[i]:=LowerCase(s);
    end;
end;


Procedure ReadImportSymbols;
var
  extlibname  : string;
  j,
  extsymcnt   : longint;
  extsymname  : string;
  extsymmangledname  : string;
  extsymordnr : longint;
  extsymisvar : boolean;
begin
  while not ppufile.endofentry do
    begin
      extlibname:=ppufile.getstring;
      extsymcnt:=ppufile.getlongint;
      writeln(['External Library: ',extlibname,' (',extsymcnt,' imports)']);
      for j:=0 to extsymcnt-1 do
        begin
          extsymname:=ppufile.getstring;
          if ppuversion>130 then
            extsymmangledname:=ppufile.getstring
          else
            extsymmangledname:=extsymname;
          extsymordnr:=ppufile.getlongint;
          extsymisvar:=ppufile.getbyte<>0;
          writeln([' ',extsymname,' as ',extsymmangledname,
            '(OrdNr: ',extsymordnr,' IsVar: ',extsymisvar,')']);
        end;
    end;
end;


Procedure ReadDerefdata;
begin
  derefdatalen:=ppufile.entrysize;
  if derefdatalen=0 then
    begin
      WriteError('!! Error: derefdatalen=0');
      exit;
    end;
  Writeln(['Derefdata length: ',derefdatalen]);
  derefdata:=allocmem(derefdatalen);
  ppufile.getdata(derefdata^,derefdatalen);
end;

Procedure FreeDerefdata;
begin
  if assigned(derefdata) then
    begin
      FreeMem(derefdata);
      derefdata:=nil;
      derefdatalen:=0;
    end;
end;


Procedure ReadWpoFileInfo;
begin
  Writeln(['Compiled with input whole-program optimisation from ',ppufile.getstring,' ',filetimestring(ppufile.getlongint)]);
end;


Procedure ReadAsmSymbols;
type
  { Copied from aasmbase.pas }
  TAsmsymbind=(
    AB_NONE,AB_EXTERNAL,AB_COMMON,AB_LOCAL,AB_GLOBAL,AB_WEAK_EXTERNAL,
    { global in the current program/library, but not visible outside it }
    AB_PRIVATE_EXTERN,AB_LAZY,AB_IMPORT);

  TAsmsymtype=(
    AT_NONE,AT_FUNCTION,AT_DATA,AT_SECTION,AT_LABEL,
    {
      the address of this code label is taken somewhere in the code
      so it must be taken care of it when creating pic
    }
    AT_ADDR
    );

var
  s,
  bindstr,
  typestr  : string;
  i : longint;
begin
  writeln([space,'Number of AsmSymbols: ',ppufile.getlongint]);
  i:=0;
  while (not ppufile.endofentry) and (not ppufile.error) do
   begin
     s:=ppufile.getstring;
     case tasmsymbind(ppufile.getbyte) of
       AB_EXTERNAL :
         bindstr:='External';
       AB_COMMON :
         bindstr:='Common';
       AB_LOCAL :
         bindstr:='Local';
       AB_GLOBAL :
         bindstr:='Global';
       AB_WEAK_EXTERNAL :
         bindstr:='Weak external';
       AB_PRIVATE_EXTERN :
         bindstr:='Private extern';
       AB_LAZY :
         bindstr:='Lazy';
       AB_IMPORT :
         bindstr:='Import';
       else
         begin
           bindstr:='<Error !!>';
           SetHasErrors;
         end;
     end;
     case tasmsymtype(ppufile.getbyte) of
       AT_FUNCTION :
         typestr:='Function';
       AT_DATA :
         typestr:='Data';
       AT_SECTION :
         typestr:='Section';
       AT_LABEL :
         typestr:='Label';
       AT_ADDR :
         typestr:='Label (with address taken)';
       else
         begin
           typestr:='<Error !!>';
           SetHasErrors;
         end;
     end;
     Writeln([space,'  ',i,' : ',s,' [',bindstr,',',typestr,']']);
     inc(i);
   end;
end;

function getexprint:Tconstexprint;

begin
  getexprint.overflow:=false;
  getexprint.signed:=boolean(ppufile.getbyte);
  getexprint.svalue:=ppufile.getint64;
end;

Procedure ReadPosInfo(Def: TPpuDef = nil);
var
  info : byte;
  fileindex,line,column : longint;
begin
  with ppufile do
   begin
     {
       info byte layout in bits:
       0-1 - amount of bytes for fileindex
       2-3 - amount of bytes for line
       4-5 - amount of bytes for column
     }
     info:=getbyte;
     case (info and $03) of
      0 : fileindex:=getbyte;
      1 : fileindex:=getword;
      2 : fileindex:=(getbyte shl 16) or getword;
      3 : fileindex:=getlongint;
     end;
     case ((info shr 2) and $03) of
      0 : line:=getbyte;
      1 : line:=getword;
      2 : line:=(getbyte shl 16) or getword;
      3 : line:=getlongint;
     end;
     case ((info shr 4) and $03) of
      0 : column:=getbyte;
      1 : column:=getword;
      2 : column:=(getbyte shl 16) or getword;
      3 : column:=getlongint;
     end;
     Writeln([fileindex,' (',line,',',column,')']);
     if Def <> nil then
       begin
         Def.FilePos.FileIndex:=fileindex - 1;
         Def.FilePos.Line:=line;
         Def.FilePos.Col:=column;
       end;
   end;
end;


procedure readderef(const derefspace: string; Ref: TPpuRef = nil);
var
  b : tdereftype;
  first : boolean;
  idx : longint;
  i,n : byte;
  pdata : pbyte;
begin
  if not assigned(derefdata) then
    exit;
  first:=true;
  idx:=ppufile.getlongint;
  if idx = -1 then
    begin
      writeln('Nil');
      exit;
    end;
  if (idx>derefdatalen) then
    begin
      WriteError('!! Error: Deref idx '+IntToStr(idx)+' > '+IntToStr(derefdatalen));
      exit;
    end;
  write([derefspace,'(',idx,') ']);
  pdata:=@derefdata[idx];
  i:=0;
  n:=pdata[i];
  inc(i);
  if n<1 then
    begin
      WriteError('!! Error: Deref len < 1');
      exit;
    end;
  while (i<=n) do
   begin
     if not first then
      write(', ')
     else
      first:=false;
     b:=tdereftype(pdata[i]);
     inc(i);
     case b of
       deref_nil :
         write('Nil');
       deref_symid :
         begin
           idx:=pdata[i] shl 24 or pdata[i+1] shl 16 or pdata[i+2] shl 8 or pdata[i+3];
           inc(i,4);
           write(['SymId ',idx]);
           if Ref <> nil then
             Ref.Id:=idx;
         end;
       deref_defid :
         begin
           idx:=pdata[i] shl 24 or pdata[i+1] shl 16 or pdata[i+2] shl 8 or pdata[i+3];
           inc(i,4);
           write(['DefId ',idx]);
           if Ref <> nil then
             Ref.Id:=idx;
         end;
       deref_unit :
         begin
           idx:=pdata[i] shl 8 or pdata[i+1];
           inc(i,2);
           write(['Unit ',idx]);
           if Ref <> nil then
             Ref.UnitIndex:=idx;
         end;
       else
         begin
           WriteError('!! unsupported dereftyp: '+IntToStr(ord(b)));
           break;
         end;
     end;
   end;
  writeln;
end;


procedure readpropaccesslist(const s:string; Ref: TPpuRef = nil);
{ type tsltype is in symconst unit }
const
  slstr : array[tsltype] of string[12] = (
    '',
    'load',
    'call',
    'subscript',
    'vec',
    'typeconv',
    'absolutetype'
  );
var
  sl : tsltype;
begin
  readderef('',Ref);
  repeat
    sl:=tsltype(ppufile.getbyte);
    if sl=sl_none then
     break;
    write([s,'(',slstr[sl],') ']);
    case sl of
      sl_call,
      sl_load,
      sl_subscript :
        if (Ref <> nil) and (Ref.IsNull) then
         begin
           readderef('',Ref);
           Ref.IsSymId:=True;
         end
        else
          readderef('');
      sl_absolutetype,
      sl_typeconv :
        readderef('');
      sl_vec :
        begin
          writeln([ppufile.getlongint]);
          readderef('');
        end;
    end;
  until false;
end;

(*
       talignmentinfo = packed record
         procalign,
         loopalign,
         jumpalign,
         constalignmin,
         constalignmax,
         varalignmin,
         varalignmax,
         localalignmin,
         localalignmax,
         recordalignmin,
         recordalignmax,
         maxCrecordalign : longint;
       end;


       tsettings = packed record
         alignment       : talignmentinfo;
         globalswitches  : tglobalswitches;
         moduleswitches  : tmoduleswitches;
         localswitches   : tlocalswitches;
         modeswitches    : tmodeswitches;
         optimizerswitches : toptimizerswitches;
         { generate information necessary to perform these wpo's during a subsequent compilation }
         genwpoptimizerswitches: twpoptimizerswitches;
         { perform these wpo's using information generated during a previous compilation }
         dowpoptimizerswitches: twpoptimizerswitches;
         debugswitches   : tdebugswitches;
         { 0: old behaviour for sets <=256 elements
           >0: round to this size }
         setalloc,
         packenum        : shortint;

         packrecords     : shortint;
         maxfpuregisters : shortint;

         cputype,
         optimizecputype : tcputype;
         fputype         : tfputype;
         asmmode         : tasmmode;
         interfacetype   : tinterfacetypes;
         defproccall     : tproccalloption;
         sourcecodepage  : tcodepagestring;

         minfpconstprec  : tfloattype;

         disabledircache : boolean;

        { CPU targets with microcontroller support can add a controller specific unit }
{$if defined(ARM) or defined(AVR)}
        controllertype   : tcontrollertype;
{$endif defined(ARM) or defined(AVR)}
         { WARNING: this pointer cannot be written as such in record token }
         pmessage : pmessagestaterecord;
       end;

*)
procedure readprocinfooptions(space : string);
(*
       tprocinfoflag=(
         { procedure has at least one assembler block }
         pi_has_assembler_block,
         { procedure does a call }
         pi_do_call,
         { procedure has a try statement = no register optimization }
         pi_uses_exceptions,
         { procedure is declared as @var(assembler), don't optimize}
         pi_is_assembler,
         { procedure contains data which needs to be finalized }
         pi_needs_implicit_finally,
         { procedure has the implicit try..finally generated }
         pi_has_implicit_finally,
         { procedure uses fpu}
         pi_uses_fpu,
         { procedure uses GOT for PIC code }
         pi_needs_got,
         { references var/proc/type/const in static symtable,
           i.e. not allowed for inlining from other units }
         pi_uses_static_symtable,
         { set if the procedure has to push parameters onto the stack }
         pi_has_stackparameter,
         { set if the procedure has at least one label }
         pi_has_label,
         { calls itself recursive }
         pi_is_recursive,
         { stack frame optimization not possible (only on x86 probably) }
         pi_needs_stackframe,
         { set if the procedure has at least one register saved on the stack }
         pi_has_saved_regs,
         { dfa was generated for this proc }
         pi_dfaavailable,
         { subroutine contains interprocedural used labels }
         pi_has_interproclabel,
         { subroutine contains interprocedural gotos }
         pi_has_global_goto
       ); *)

type
  tprocinfoopt=record
    mask : tprocinfoflag;
    str  : string[81];
  end;
const
  procinfoopts=ord(high(tprocinfoflag)) - ord(low(tprocinfoflag));
  procinfoopt : array[0..procinfoopts] of tprocinfoopt=(
         (mask:pi_has_assembler_block;
         str:' has at least one assembler block'),
         (mask:pi_do_call;
         str:' does a call'),
         (mask:pi_uses_exceptions;
         str:' has a try statement = no register optimization '),
         (mask:pi_is_assembler;
         str:' is declared as @var(assembler), don''t optimize'),
         (mask:pi_needs_implicit_finally;
         str:' contains data which needs to be finalized '),
         (mask:pi_has_implicit_finally;
         str:' has the implicit try..finally generated '),
         (mask:pi_uses_fpu;
         str:' uses fpu'),
         (mask:pi_needs_got;
         str:' uses GOT for PIC code '),
         (mask:pi_uses_static_symtable;
         str:' references var/proc/type/const in static symtable'),
         (mask:pi_has_stackparameter;
         str:' set if the procedure has to push parameters onto the stack '),
         (mask:pi_has_label;
         str:' set if the procedure has at least one label '),
         (mask:pi_is_recursive;
         str:' calls itself recursive '),
         (mask:pi_needs_stackframe;
         str:' stack frame optimization not possible (only on x86 probably) '),
         (mask:pi_has_saved_regs;
         str:' set if the procedure has at least one register saved on the stack '),
         (mask:pi_dfaavailable;
         str:' dfa was generated for this proc '),
         (mask:pi_has_interproclabel;
         str:' subroutine contains interprocedural used labels '),
         (mask:pi_has_unwind_info;
         str:' unwinding info was generated for this proc '),
         (mask:pi_has_global_goto;
         str:' subroutine contains interprocedural goto '),
         (mask:pi_has_inherited;
         str:' subroutine contains inherited call '),
         (mask:pi_has_nested_exit;
         str:' subroutine contains a nested subroutine which calls the exit of the current one '),
         (mask:pi_has_stack_allocs;
         str:' allocates memory on stack, so stack may be unbalanced on exit ')

  );
var
  procinfooptions : tprocinfoflags;
  i      : longint;
  first  : boolean;
begin
  ppufile.getsmallset(procinfooptions);
  if procinfooptions<>[] then
   begin
     first:=true;
     for i:=0 to procinfoopts do
      if (procinfoopt[i].mask in procinfooptions) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(procinfoopt[i].str);
       end;
   end;
  writeln;
end;

procedure readsymoptions(space : string; Def: TPpuDef = nil);
type
  tsymopt=record
    mask : tsymoption;
    str  : string[30];
  end;
const
  symopts=ord(high(tsymoption)) - ord(low(tsymoption));
  { sp_none = 0 corresponds to nothing }
  symopt : array[1..symopts] of tsymopt=(
     (mask:sp_static;             str:'Static'),
     (mask:sp_hint_deprecated;    str:'Hint Deprecated'),
     (mask:sp_hint_platform;      str:'Hint Platform'),
     (mask:sp_hint_library;       str:'Hint Library'),
     (mask:sp_hint_unimplemented; str:'Hint Unimplemented'),
     (mask:sp_hint_experimental;  str:'Hint Experimental'),
     (mask:sp_has_overloaded;     str:'Has overloaded'),
     (mask:sp_internal;           str:'Internal'),
     (mask:sp_implicitrename;     str:'Implicit Rename'),
     (mask:sp_generic_para;       str:'Generic Parameter'),
     (mask:sp_has_deprecated_msg; str:'Has Deprecated Message'),
     (mask:sp_generic_dummy;      str:'Generic Dummy'),
     (mask:sp_explicitrename;     str:'Explicit Rename')
  );
var
  symoptions : tsymoptions;
  i      : longint;
  first  : boolean;
begin
  ppufile.getsmallset(symoptions);
  if symoptions<>[] then
   begin
     if Def <> nil then
       if sp_internal in symoptions then
         Def.Visibility:=dvHidden;
     first:=true;
     for i:=1to symopts do
      if (symopt[i].mask in symoptions) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(symopt[i].str);
       end;
   end;
  writeln;
  if sp_has_deprecated_msg in symoptions then
    writeln([space,'Deprecated : ', ppufile.getstring]);
end;

procedure readvisibility(Def: TPpuDef = nil);
var
  i: byte;
begin
  i:=ppufile.getbyte;
  if Def <> nil then
    case tvisibility(i) of
      vis_public: Def.Visibility:=dvPublic;
      vis_published: Def.Visibility:=dvPublished;
      vis_protected, vis_strictprotected: Def.Visibility:=dvProtected;
      else Def.Visibility:=dvPrivate;
    end;
  writeln(Visibility2Str(i));
end;

procedure readcommonsym(const s:string; Def: TPpuDef = nil);
var
  i: integer;
  n: string;
begin
  i:=ppufile.getlongint;
  if Def <> nil then
    Def.SetSymId(i);
  writeln([space,'** Symbol Id ',i,' **']);
  n:=ppufile.getstring;
  if Def <> nil then
    Def.Name:=n;
  writeln([space,s,n]);
  write  ([space,'     File Pos : ']);
  readposinfo(Def);
  write  ([space,'   Visibility : ']);
  readvisibility(Def);
  write  ([space,'   SymOptions : ']);
  readsymoptions(space+'   ',Def);
end;



var
  { needed during tobjectdef parsing... }
  current_defoptions : tdefoptions;
  current_objectoptions : tobjectoptions;

procedure readcommondef(const s:string; out defoptions: tdefoptions; Def: TPpuDef = nil);
type
  tdefopt=record
    mask : tdefoption;
    str  : string[30];
  end;
  tdefstateinfo=record
    mask : tdefstate;
    str  : string[30];
  end;
  tgenconstrflag=record
    mask : tgenericconstraintflag;
    str  : string[30];
  end;
  ptoken=^ttoken;
  pmsgstate =^tmsgstate;
const
  defopt : array[1..ord(high(tdefoption))] of tdefopt=(
     (mask:df_unique;         str:'Unique Type'),
     (mask:df_generic;        str:'Generic'),
     (mask:df_specialization; str:'Specialization'),
     (mask:df_copied_def;     str:'Copied Typedef'),
     (mask:df_genconstraint;  str:'Generic Constraint')
  );
  defstate : array[1..ord(high(tdefstate))] of tdefstateinfo=(
     (mask:ds_vmt_written;           str:'VMT Written'),
     (mask:ds_rtti_table_used;       str:'RTTITable Used'),
     (mask:ds_init_table_used;       str:'InitTable Used'),
     (mask:ds_rtti_table_written;    str:'RTTITable Written'),
     (mask:ds_init_table_written;    str:'InitTable Written'),
     (mask:ds_dwarf_dbg_info_used;   str:'Dwarf DbgInfo Used'),
     (mask:ds_dwarf_dbg_info_written;str:'Dwarf DbgInfo Written')
  );
  genconstrflag : array[1..ord(high(tgenericconstraintflag))] of tgenconstrflag=(
     (mask:gcf_constructor; str:'Constructor'),
     (mask:gcf_class;       str:'Class'),
     (mask:gcf_record;      str:'Record')
  );
var
  defstates  : tdefstates;
  i, nb{, msgvalue}, mesgnb : longint;
  first  : boolean;
  copy_size, min_size, tokenbufsize : longint;
  tokenbuf : pbyte;
//  idtoken,
  token : ttoken;
//  state : tmsgstate;
  new_settings : Tsettings;
  len : sizeint;
  wstring : widestring;
  astring : ansistring;
  genconstr : tgenericconstraintflags;

  function readtoken: ttoken;
    var
      b,b2 : byte;
    begin
      b:=tokenbuf[i];
      inc(i);
      if (b and $80)<>0 then
        begin
          b2:=tokenbuf[i];
          inc(i);
          result:=ttoken(((b and $7f) shl 8) or b2);
        end
      else
        result:=ttoken(b);
    end;

  function gettokenbufdword : dword;
  var
    var32 : dword;
  begin
    var32:=pdword(@tokenbuf[i])^;
    inc(i,sizeof(dword));
    if ppufile.change_endian then
      var32:=swapendian(var32);
    result:=var32;
  end;

  function gettokenbufword : word;
  var
    var16 : word;
  begin
    var16:=pword(@tokenbuf[i])^;
    inc(i,sizeof(word));
    if ppufile.change_endian then
      var16:=swapendian(var16);
    result:=var16;
  end;


  function gettokenbufsizeint : int64;
  var
    var64 : int64;
    var32 : longint;
    var16 : smallint;

  begin
    if CpuAddrBitSize[cpu]=64 then
      begin
        var64:=pint64(@tokenbuf[i])^;
        inc(i,sizeof(int64));
        if ppufile.change_endian then
          var64:=swapendian(var64);
        result:=var64;
      end
    else if CpuAddrBitSize[cpu]=32 then
      begin
        var32:=plongint(@tokenbuf[i])^;
        inc(i,sizeof(longint));
        if ppufile.change_endian then
          var32:=swapendian(var32);
        result:=var32;
      end
    else if CpuAddrBitSize[cpu]=16 then
      begin
        var16:=psmallint(@tokenbuf[i])^;
        inc(i,sizeof(smallint));
        if ppufile.change_endian then
          var16:=swapendian(var16);
        result:=var16;
      end
    else
      begin
        WriteError('Wrong CpuAddrBitSize');
        result:=0;
      end;
  end;

begin
  i:=ppufile.getlongint;
  if Def <> nil then
    Def.Id:=i;
  writeln([space,'** Definition Id ',i,' **']);
  writeln([space,s]);
  write  ([space,'      Type symbol : ']);
  if Def <> nil then
    readderef('', Def.Ref)
  else
    readderef('');
  write  ([space,'       DefOptions : ']);
  ppufile.getsmallset(defoptions);
  if defoptions<>[] then
    begin
      first:=true;
      for i:=1to high(defopt) do
       if (defopt[i].mask in defoptions) then
        begin
          if first then
            first:=false
          else
            write(', ');
          write(defopt[i].str);
        end;
    end;
  writeln;

  write  ([space,'        DefStates : ']);
  ppufile.getsmallset(defstates);
  if defstates<>[] then
    begin
      first:=true;
      for i:=1 to high(defstate) do
       if (defstate[i].mask in defstates) then
        begin
          if first then
            first:=false
          else
            write(', ');
          write(defstate[i].str);
        end;
    end;
  writeln;

  if df_genconstraint in defoptions then
    begin
      ppufile.getsmallset(genconstr);
      write  ([space,'   GenConstraints : ']);
      if genconstr<>[] then
        begin
          first:=true;
          for i:=1 to high(genconstrflag) do
           if (genconstrflag[i].mask in genconstr) then
            begin
              if first then
                first:=false
              else
                write(', ');
              write(genconstrflag[i].str);
            end;
        end;
      writeln;

      len:=ppufile.getasizeint;
      if len>0 then
        begin
          space:='    '+space;
          writeln([space,'------ constraint defs begin ------']);
          for i:=0 to len-1 do
            begin
              writeln([space,'------ constraint def ',i,' ------']);
              readderef(space);
            end;
          writeln([space,'------ constraint defs end ------']);
          delete(space,1,4);
        end;
    end;

  if df_generic in defoptions then
    begin
      tokenbufsize:=ppufile.getlongint;
      writeln([space,' Tokenbuffer size : ',tokenbufsize]);
      tokenbuf:=allocmem(tokenbufsize);
      ppufile.getdata(tokenbuf^,tokenbufsize);
      i:=0;
      write([space,' Tokens: ']);
      while i<tokenbufsize do
        begin
          token:=readtoken;
          if token<>_GENERICSPECIALTOKEN then
            begin
              if token <= high(ttoken) then
                write(arraytokeninfo[token].str)
              else
                begin
                  HasMoreInfos;
                  write('Error in Token List');
                  break;
                end;
              {idtoken:=}readtoken;
            end;
          case token of
            _CWCHAR,
            _CWSTRING :
              begin
                len:=gettokenbufsizeint;
                setlength(wstring,len);
                move(tokenbuf[i],wstring[1],len*2);
                write([' ',wstring]);
                inc(i,len*2);
              end;
            _CSTRING:
              begin
                len:=gettokenbufsizeint;
                setlength(astring,len);
                move(tokenbuf[i],astring[1],len);
                write([' ',astring]);
                inc(i,len);
              end;
            _CCHAR,
            _INTCONST,
            _REALNUMBER :
              begin
                write([' ',pshortstring(@tokenbuf[i])^]);
                inc(i,tokenbuf[i]+1);
              end;
            _ID :
              begin
                write([' ',pshortstring(@tokenbuf[i])^]);
                inc(i,tokenbuf[i]+1);
              end;
            _GENERICSPECIALTOKEN:
              begin
                { Short version of column change,
                  byte or $80 used }
                if (tokenbuf[i] and $80)<>0 then
                  begin
                    write(['Col: ',tokenbuf[i] and $7f]);
                    inc(i);
                  end
                else
                  case tspecialgenerictoken(tokenbuf[i]) of
                    ST_LOADSETTINGS:
                      begin
                        inc(i);
                        write('Settings');
                        { This does not load pmessage pointer }
                        new_settings.pmessage:=nil;
                        { TSettings size depends in target...
                          We first read the size of the copied part }
                        { Still not cross endian ready :( }
                        copy_size:=gettokenbufsizeint;
                        if copy_size < sizeof(tsettings)-sizeof(pointer) then
                          min_size:=copy_size
                        else
                          min_size:= sizeof(tsettings)-sizeof(pointer);
                        move(tokenbuf[i],new_settings, min_size);
                        inc(i,copy_size);
                      end;
                    ST_LOADMESSAGES:
                      begin
                        inc(i);
                        write('Messages:');
                        mesgnb:=tokenbuf[i];
                        inc(i);
                        for nb:=1 to mesgnb do
                          begin
                            {msgvalue:=}gettokenbufsizeint;
                            inc(i,sizeof(sizeint));
                            //state:=tmsgstate(gettokenbufsizeint);
                          end;
                      end;
                    ST_LINE:
                      begin
                        inc(i);
                        write(['Line: ',gettokenbufdword]);
                      end;
                    ST_COLUMN:
                      begin
                        inc(i);
                        write(['Col: ',gettokenbufword]);
                      end;
                    ST_FILEINDEX:
                      begin
                        inc(i);
                        write(['File: ',gettokenbufword]);
                      end;
                  end;
              end;
          end;

          if i<tokenbufsize then
            write(',');
        end;
      writeln;
      freemem(tokenbuf);
    end;
  if df_specialization in defoptions then
    begin
      write  ([space,' Orig. GenericDef : ']);
      readderef('');
    end;
  current_defoptions:=defoptions;
end;


{ Read abstract procdef and return if inline procdef }
{ type tproccalloption is in globtype unit }
{ type tproctypeoption is in globtype unit }
{ type tprocoption is in globtype unit }

procedure read_abstract_proc_def(var proccalloption:tproccalloption;var procoptions:tprocoptions; ProcDef: TPpuProcDef);
type
  tproccallopt=record
    mask : tproccalloption;
    str  : string[30];
  end;
  tproctypeopt=record
    mask : tproctypeoption;
    str  : string[30];
  end;
  tprocopt=record
    mask : tprocoption;
    str  : string[31];
  end;
const
  {proccalloptionStr  is also in globtype unit }
  proctypeopt : array[1..ord(high(tproctypeoption))] of tproctypeopt=(
     (mask:potype_proginit;          str:'ProgInit'),
     (mask:potype_unitinit;          str:'UnitInit'),
     (mask:potype_unitfinalize;      str:'UnitFinalize'),
     (mask:potype_constructor;       str:'Constructor'),
     (mask:potype_destructor;        str:'Destructor'),
     (mask:potype_operator;          str:'Operator'),
     (mask:potype_procedure;         str:'Procedure'),
     (mask:potype_function;          str:'Function'),
     (mask:potype_class_constructor; str:'Class Constructor'),
     (mask:potype_class_destructor;  str:'Class Destructor'),
     { Dispinterface property accessors }
     (mask:potype_propgetter;        str:'Property Getter'),
     (mask:potype_propsetter;        str:'Property Setter'),
     (mask:potype_exceptfilter;      str:'SEH filter')
  );
  procopt : array[1..ord(high(tprocoption))] of tprocopt=(
     (mask:po_classmethod;     str:'ClassMethod'),
     (mask:po_virtualmethod;   str:'VirtualMethod'),
     (mask:po_abstractmethod;  str:'AbstractMethod'),
     (mask:po_finalmethod;     str:'FinalMethod'),
     (mask:po_staticmethod;    str:'StaticMethod'),
     (mask:po_overridingmethod;str:'OverridingMethod'),
     (mask:po_methodpointer;   str:'MethodPointer'),
     (mask:po_interrupt;       str:'Interrupt'),
     (mask:po_iocheck;         str:'IOCheck'),
     (mask:po_assembler;       str:'Assembler'),
     (mask:po_msgstr;          str:'MsgStr'),
     (mask:po_msgint;          str:'MsgInt'),
     (mask:po_exports;         str:'Exports'),
     (mask:po_external;        str:'External'),
     (mask:po_overload;        str:'Overload'),
     (mask:po_varargs;         str:'VarArgs'),
     (mask:po_internconst;     str:'InternConst'),
     (mask:po_addressonly;     str:'AddressOnly'),
     (mask:po_public;          str:'Public'),
     (mask:po_hascallingconvention;str:'HasCallingConvention'),
     (mask:po_reintroduce;     str:'ReIntroduce'),
     (mask:po_explicitparaloc; str:'ExplicitParaloc'),
     (mask:po_nostackframe;    str:'NoStackFrame'),
     (mask:po_has_mangledname; str:'HasMangledName'),
     (mask:po_has_public_name; str:'HasPublicName'),
     (mask:po_forward;         str:'Forward'),
     (mask:po_global;          str:'Global'),
     (mask:po_has_inlininginfo;str:'HasInliningInfo'),
     (mask:po_syscall_legacy;  str:'SyscallLegacy'),
     (mask:po_syscall_sysv;    str:'SyscallSysV'),
     (mask:po_syscall_basesysv;str:'SyscallBaseSysV'),
     (mask:po_syscall_sysvbase;str:'SyscallSysVBase'),
     (mask:po_syscall_r12base; str:'SyscallR12Base'),
     (mask:po_inline;          str:'Inline'),
     (mask:po_compilerproc;    str:'CompilerProc'),
     (mask:po_has_importdll;   str:'HasImportDLL'),
     (mask:po_has_importname;  str:'HasImportName'),
     (mask:po_kylixlocal;      str:'KylixLocal'),
     (mask:po_dispid;          str:'DispId'),
     (mask:po_weakexternal;    str:'WeakExternal'),
     (mask:po_objc;            str:'ObjC'),
     (mask:po_enumerator_movenext; str:'EnumeratorMoveNext'),
     (mask:po_optional;        str: 'Optional'),
     (mask:po_delphi_nested_cc;str: 'Delphi-style nested frameptr'),
     (mask:po_java_nonvirtual; str: 'Java non-virtual method'),
     (mask:po_ignore_for_overload_resolution;str: 'Ignored for overload resolution'),
     (mask:po_rtlproc;         str: 'RTL procedure'),
     (mask:po_auto_raised_visibility; str: 'Visibility raised by compiler'),
     (mask:po_far;             str: 'Far'),
     (mask:po_noreturn;             str: 'No return')
  );
var
  proctypeoption  : tproctypeoption;
  i     : longint;
  first : boolean;
  tempbuf : array[0..255] of byte;
begin
  write([space,'      Return type : ']);
  readderef('', ProcDef.ReturnType);
  writeln([space,'         Fpu used : ',ppufile.getbyte]);
  proctypeoption:=tproctypeoption(ppufile.getbyte);
  case proctypeoption of
    potype_function: Include(ProcDef.Options, poFunction);
    potype_procedure: Include(ProcDef.Options, poProcedure);
    potype_constructor: Include(ProcDef.Options, poConstructor);
    potype_destructor: Include(ProcDef.Options, poDestructor);
    potype_operator: Include(ProcDef.Options, poOperator);
  end;
  write([space,'       TypeOption : ']);
  first:=true;
  for i:=1 to high(proctypeopt) do
   if (proctypeopt[i].mask=proctypeoption) then
    begin
      if first then
        first:=false
      else
        write(', ');
      write(proctypeopt[i].str);
    end;
  writeln;
  proccalloption:=tproccalloption(ppufile.getbyte);
  writeln([space,'       CallOption : ',proccalloptionStr[proccalloption]]);
  ppufile.getnormalset(procoptions);
  if procoptions<>[] then
   begin
     if po_classmethod in procoptions then Include(ProcDef.Options, poClassMethod);
     if po_virtualmethod in procoptions then Include(ProcDef.Options, poVirtual);
     if po_abstractmethod in procoptions then Include(ProcDef.Options, poAbstract);
     if po_overridingmethod in procoptions then Include(ProcDef.Options, poOverriding);
     if po_overload in procoptions then Include(ProcDef.Options, poOverload);
     if po_inline in procoptions then Include(ProcDef.Options, poInline);
     if (po_methodpointer in procoptions) and (ProcDef.DefType = dtProcType) then
       TPpuProcTypeDef(ProcDef).MethodPtr:=True;

     write([space,'          Options : ']);
     first:=true;
     for i:=1 to high(procopt) do
      if (procopt[i].mask in procoptions) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(procopt[i].str);
       end;
     writeln;
   end;
  if (po_explicitparaloc in procoptions) then
    begin
      i:=ppufile.getbyte;
      ppufile.getdata(tempbuf,i);
    end;
end;


{ type tvaroption is in unit symconst }
  { register variable }
{ type tvarregable is in unit symconst }
procedure readabstractvarsym(const s:string;var varoptions:tvaroptions; VarDef: TPpuVarDef = nil);
type
  tvaropt=record
    mask : tvaroption;
    str  : string[30];
  end;
const
  varopt : array[1..ord(high(tvaroption))] of tvaropt=(
     (mask:vo_is_external;     str:'External'),
     (mask:vo_is_dll_var;      str:'DLLVar'),
     (mask:vo_is_thread_var;   str:'ThreadVar'),
     (mask:vo_has_local_copy;  str:'HasLocalCopy'),
     (mask:vo_is_const;        str:'Constant'),
     (mask:vo_is_public;       str:'Public'),
     (mask:vo_is_high_para;    str:'HighValue'),
     (mask:vo_is_funcret;      str:'Funcret'),
     (mask:vo_is_self;         str:'Self'),
     (mask:vo_is_vmt;          str:'VMT'),
     (mask:vo_is_result;       str:'Result'),
     (mask:vo_is_parentfp;     str:'ParentFP'),
     (mask:vo_is_loop_counter; str:'LoopCounter'),
     (mask:vo_is_hidden_para;  str:'Hidden'),
     (mask:vo_has_explicit_paraloc;str:'ExplicitParaloc'),
     (mask:vo_is_syscall_lib;  str:'SysCallLib'),
     (mask:vo_has_mangledname; str:'HasMangledName'),
     (mask:vo_is_typed_const;  str:'TypedConst'),
     (mask:vo_is_range_check;  str:'RangeCheckSwitch'),
     (mask:vo_is_overflow_check; str:'OverflowCheckSwitch'),
     (mask:vo_is_typinfo_para; str:'TypeInfo'),
     (mask:vo_is_msgsel;str:'MsgSel'),
     (mask:vo_is_weak_external;str:'WeakExternal'),
     (mask:vo_is_first_field;str:'IsFirstField'),
     (mask:vo_volatile;        str:'Volatile'),
     (mask:vo_has_section;     str:'HasSection'),
     (mask:vo_force_finalize;  str:'ForceFinalize'),
     (mask:vo_is_default_var;  str:'DefaultIntrinsicVar')
  );
var
  i : longint;
  first : boolean;
begin
  readcommonsym(s, VarDef);
  i:=ppufile.getbyte;
  if (VarDef <> nil) and (VarDef.DefType = dtParam) then
    with TPpuParamDef(VarDef) do
      case tvarspez(i) of
        vs_value: Spez:=psValue;
        vs_var: Spez:=psVar;
        vs_out: Spez:=psOut;
        vs_const: Spez:=psConst;
        vs_constref: Spez:=psConstRef;
      end;
  writeln([space,'         Spez : ',Varspez2Str(i)]);
  writeln([space,'      Regable : ',Varregable2Str(ppufile.getbyte)]);
  writeln([space,'   Addr Taken : ',(ppufile.getbyte<>0)]);
  write  ([space,'     Var Type : ']);
  if VarDef <> nil then
    readderef('',VarDef.VarType)
  else
    readderef('');
  ppufile.getsmallset(varoptions);
  if varoptions<>[] then
   begin
     if (VarDef <> nil) and (VarDef.DefType = dtParam) and (vo_is_hidden_para in varoptions) then
       TPpuParamDef(VarDef).Spez:=psHidden;
     write([space,'      Options : ']);
     first:=true;
     for i:=1 to high(varopt) do
      if (varopt[i].mask in varoptions) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(varopt[i].str);
         if varopt[i].mask = vo_has_section then
           writeln(['Section name:',ppufile.getansistring]);
       end;
     writeln;
   end;
end;


procedure readobjectdefoptions(ObjDef: TPpuObjectDef = nil);
type
  tsymopt=record
    mask : tobjectoption;
    str  : string[30];
  end;
const
  symopt : array[1..ord(high(tobjectoption))] of tsymopt=(
     (mask:oo_is_forward;         str:'IsForward'),
     (mask:oo_is_abstract;        str:'IsAbstract'),
     (mask:oo_is_sealed;          str:'IsSealed'),
     (mask:oo_has_virtual;        str:'HasVirtual'),
     (mask:oo_has_private;        str:'HasPrivate'),
     (mask:oo_has_protected;      str:'HasProtected'),
     (mask:oo_has_strictprivate;  str:'HasStrictPrivate'),
     (mask:oo_has_strictprotected;str:'HasStrictProtected'),
     (mask:oo_has_constructor;    str:'HasConstructor'),
     (mask:oo_has_destructor;     str:'HasDestructor'),
     (mask:oo_has_vmt;            str:'HasVMT'),
     (mask:oo_has_msgstr;         str:'HasMsgStr'),
     (mask:oo_has_msgint;         str:'HasMsgInt'),
     (mask:oo_can_have_published; str:'CanHavePublished'),
     (mask:oo_has_default_property;str:'HasDefaultProperty'),
     (mask:oo_has_valid_guid;     str:'HasValidGUID'),
     (mask:oo_has_enumerator_movenext; str:'HasEnumeratorMoveNext'),
     (mask:oo_has_enumerator_current;  str:'HasEnumeratorCurrent'),
     (mask:oo_is_external;        str:'External'),
     (mask:oo_is_formal;          str:'Formal'),
     (mask:oo_is_classhelper;     str:'Class Helper/Category'),
     (mask:oo_has_class_constructor; str:'HasClassConstructor'),
     (mask:oo_has_class_destructor; str:'HasClassDestructor'),
     (mask:oo_is_enum_class;      str:'JvmEnumClass'),
     (mask:oo_has_new_destructor; str:'HasNewDestructor')
  );
var
  i      : longint;
  first  : boolean;
begin
  ppufile.getsmallset(current_objectoptions);
  if current_objectoptions<>[] then
   begin
     if ObjDef <> nil then
      begin
        if oo_is_abstract in current_objectoptions then
          Include(ObjDef.Options, ooIsAbstract);
      end;
     first:=true;
     for i:=1 to high(symopt) do
      if (symopt[i].mask in current_objectoptions) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(symopt[i].str);
       end;
   end;
  writeln;
end;


procedure readarraydefoptions(ArrayDef: TPpuArrayDef);
{ type tarraydefoption is in unit symconst }
const
  symopt : array[tarraydefoption] of string = (
   { ado_IsConvertedPointer } 'ConvertedPointer',
   { ado_IsDynamicArray     } 'IsDynamicArray',
   { ado_IsVariant          } 'IsVariant',
   { ado_IsConstructor      } 'IsConstructor',
   { ado_IsArrayOfConst     } 'ArrayOfConst',
   { ado_IsConstString      } 'ConstString',
   { ado_IsBitPacked        } 'BitPacked'
  );
var
  symoptions: tarraydefoptions;
  i: tarraydefoption;
  first: boolean;
begin
  ppufile.getsmallset(symoptions);
  if symoptions<>[] then
   begin
     if ado_IsDynamicArray in symoptions then Include(ArrayDef.Options, aoDynamic);
     first:=true;
     for i:=Low(symopt) to high(symopt) do
      if (i in symoptions) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(symopt[i]);
       end;
   end;
  writeln;
end;

(* options for properties
  tpropertyoption=(ppo_none,
    ppo_indexed,
    ppo_defaultproperty,
    ppo_stored,
    ppo_hasparameters,
    ppo_implements,
    ppo_enumerator_current,
    ppo_overrides,
    ppo_dispid_write              { no longer used }
  );
  tpropertyoptions=set of tpropertyoption;
*)
function readpropertyoptions:tpropertyoptions;
{ type tarraydefoption is in unit symconst }
type
  tpropopt=record
    mask : tpropertyoption;
    str  : string[30];
  end;
const
  symopt : array[1..ord(high(tpropertyoption))] of tpropopt=(
    (mask:ppo_indexed;str:'indexed'),
    (mask:ppo_defaultproperty;str:'default'),
    (mask:ppo_stored;str:'stored'),
    (mask:ppo_hasparameters;str:'has parameters'),
    (mask:ppo_implements;str:'implements'),
    (mask:ppo_enumerator_current;str:'enumerator current'),
    (mask:ppo_overrides;str:'overrides'),
    (mask:ppo_dispid_write;str:'dispid write')  { no longer used }
  );
var
  i      : longint;
  first  : boolean;
begin
  ppufile.getsmallset(result);
  if result<>[] then
   begin
     first:=true;
     for i:=1 to high(symopt) do
      if (symopt[i].mask in result) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(symopt[i].str);
       end;
   end;
  writeln;
end;


procedure readnodetree;
var
  l : longint;
  p : pointer;
begin
  with ppufile do
   begin
     if space<>'' then
      Writeln([space,'------ nodetree ------']);
     if readentry=ibnodetree then
      begin
        l:=entrysize;
        Writeln([space,'Tree size : ',l]);
        { Read data to prevent error that entry is not completly read }
        getmem(p,l);
        getdata(p^,l);
        freemem(p);
      end
     else
      begin
        WriteError('!! ibnodetree not found');
      end;
   end;
end;


procedure ReadCreatedObjTypes;
var
  i,j,
  len,
  bssize: longint;
  bs: pbyte;
begin
  if ppufile.readentry<>ibcreatedobjtypes then
    begin
      WriteError('!! ibcreatedobjtypes entry not found');
      ppufile.skipdata(ppufile.entrysize);
      exit
    end;
  writeln;
  writeln([space,'WPO info']);
  writeln([space,'--------']);

  len:=ppufile.getlongint;
  writeln([space,'** Instantiated Object/Class types: ',len,' **']);
  space:=space+'  ';
  for i:=0 to len-1 do
    readderef(space);
  setlength(space,length(space)-2);

  len:=ppufile.getlongint;
  writeln([space,'** Instantiated ClassRef types: ',len,' **']);
  space:=space+'  ';
  for i:=0 to len-1 do
    readderef(space);
  setlength(space,length(space)-2);

  len:=ppufile.getlongint;
  writeln([space,'** Possibly instantiated ClassRef types : ',len,' **']);
  space:=space+'  ';
  for i:=0 to len-1 do
    readderef(space);
  setlength(space,length(space)-2);

  len:=ppufile.getlongint;
  writeln([space,'** Class types with called virtual methods info : ',len,' **']);
  space:=space+'  ';
  for i:=0 to len-1 do
    begin
      write([space,'Class def : ']);
      readderef('');
      write([space+'  ','Called vmtentries : ']);
      bssize:=ppufile.getlongint;
      getmem(bs,bssize);
      ppufile.readdata(bs^,bssize);
      for j:=0 to bssize*8-1 do
        if (((bs+j shr 3)^ shr (j and 7)) and 1) <> 0 then
          write([j,', ']);
      writeln;
      freemem(bs);
    end;
  setlength(space,length(space)-2);
end;

{****************************************************************************
                             Read Symbols Part
****************************************************************************}

procedure readsymbols(const s:string; ParentDef: TPpuContainerDef = nil);

  function _finddef(symdef: TPpuDef): TPpuDef;
  begin
    Result:=nil;
    if symdef.Ref.IsCurUnit then
     begin;
       Result:=CurUnit.FindById(symdef.Ref.Id);
       if (Result <> nil) and (Result.Ref.Id = symdef.Id) then
        begin
          Result.Name:=symdef.Name;
          Result.FilePos:=symdef.FilePos;
        end
       else
         Result:=nil;
     end;
  end;

type
  pguid = ^tguid;
  tguid = packed record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
  end;

var
  b      : byte;
  pc     : pchar;
  ch : dword;
  startnewline : boolean;
  i,j,len : longint;
  prettyname, ss : ansistring;
  ws: widestring;
  guid : tguid;
  realvalue : ppureal;
  doublevalue : double;
  singlevalue : single;
  extended : TSplit80bitReal;
  tempbuf : array[0..127] of char;
  pw : pcompilerwidestring;
  varoptions : tvaroptions;
  propoptions : tpropertyoptions;
  iexp: Tconstexprint;
  def: TPpuDef;
  constdef: TPpuConstDef absolute def;
begin
  with ppufile do
   begin
     if space<>'' then
      Writeln([space,'------ ',s,' ------']);
     if readentry=ibstartsyms then
      begin
        Writeln([space,'Symtable datasize : ',getlongint]);
        Writeln([space,'Symtable alignment: ',getlongint]);
      end
     else
      Writeln('!! ibstartsym not found');
     repeat
       def:=nil;
       b:=readentry;
       case b of

         ibunitsym :
           readcommonsym('Unit symbol ');

         ibnamespacesym :
           begin
             readcommonsym('NameSpace symbol ');
             write([space,'  Hidden Unit : ']);
             readderef('');
           end;

         iblabelsym :
           readcommonsym('Label symbol ');

         ibtypesym :
           begin
             def:=TPpuTypeRef.Create(nil);
             readcommonsym('Type symbol ',def);
             write([space,'  Result Type : ']);
             readderef('', def.Ref);
             if _finddef(def) = nil then
               def.Parent:=ParentDef;
             prettyname:=getansistring;
             if prettyname<>'' then
               begin
                 write([space,' Pretty Name : ']);
                 Writeln(prettyname);
               end;
           end;

         ibprocsym :
           begin
             def:=TPpuDef.Create(nil);
             readcommonsym('Procedure symbol ', def);
             len:=ppufile.getword;
             for i:=1 to len do
              begin
                write([space,'   Definition : ']);
                readderef('', def.Ref);
                _finddef(def);
              end;
           end;

         ibconstsym :
           begin
             constdef:=TPpuConstDef.Create(ParentDef);
             readcommonsym('Constant symbol ',constdef);
             b:=getbyte;
             case tconsttyp(b) of
               constord :
                 begin
                   write  ([space,'  OrdinalType : ']);
                   readderef('',constdef.TypeRef);
                   iexp:=getexprint;
                   constdef.ConstType:=ctInt;
                   constdef.VInt:=iexp.svalue;
                   writeln([space,'        Value : ',constexp.tostr(iexp)]);
                 end;
               constpointer :
                 begin
                   write  ([space,'  PointerType : ']);
                   readderef('',constdef.TypeRef);
                   constdef.ConstType:=ctInt;
                   constdef.VInt:=getaint;
                   writeln([space,'        Value : ',constdef.VInt])
                 end;
               conststring,
               constresourcestring :
                 begin
                   len:=getlongint;
                   getmem(pc,len+1);
                   getdata(pc^,len);
                   (pc+len)^:= #0;
                   writeln([space,'       Length : ',len]);
                   writeln([space,'        Value : "',pc,'"']);
                   constdef.ConstType:=ctStr;
                   SetString(constdef.VStr, pc, len);
                   constdef.VStr:=UTF8Encode(constdef.VStr);
                   freemem(pc,len+1);
                 end;
               constreal :
                 begin
                   constdef.ConstType:=ctFloat;
                   write  ([space,'     RealType : ']);
                   readderef('',constdef.TypeRef);
                   write([space,'        Value : ']);
                   if entryleft=sizeof(ppureal) then
                     begin
                       realvalue:=getrealsize(sizeof(ppureal));
                       constdef.VFloat:=realvalue;
                       writeln([realvalue]);
                     end
                   else if entryleft=sizeof(double) then
                     begin
                       doublevalue:=getrealsize(sizeof(double));
                       constdef.VFloat:=doublevalue;
                       writeln([doublevalue]);
                     end
                   else if entryleft=sizeof(single) then
                     begin
                       singlevalue:=getrealsize(sizeof(single));
                       constdef.VFloat:=singlevalue;
                       writeln([singlevalue]);
                     end
                   else if entryleft=10 then
                     begin
                       getdata(extended,entryleft);
                       ss:=Real80bitToStr(extended);
                       constdef.VFloat:=StrToFloat(ss);
                       writeln(ss);
                     end
                   else
                     begin
                       realvalue:=0.0;
                       WriteError('Error reading real value');
                     end;
                 end;
               constset :
                 begin
                   constdef.ConstType:=ctSet;
                   write ([space,'      Set Type : ']);
                   readderef('',constdef.TypeRef);
                   for i:=1to 4 do
                    begin
                      write ([space,'        Value : ']);
                      for j:=1to 8 do
                       begin
                         if j>1 then
                          write(',');
                         b:=getbyte;
                         write(hexstr(b,2));
                         constdef.VSet[i*j-1]:=b;
                       end;
                      writeln;
                    end;
                 end;
               constnil:
                 begin
                   writeln([space,' NIL pointer.']);
                   constdef.ConstType:=ctPtr;
                   constdef.VInt:=0;
                 end;
               constwstring :
                 begin
                   initwidestring(pw);
                   setlengthwidestring(pw,getlongint);
                   if widecharsize=2 then
                   { don't use getdata, because the compilerwidechars may have to
                     be byteswapped
                   }
                     begin
                       for i:=0 to pw^.len-1 do
                         pw^.data[i]:=ppufile.getword;
                       SetString(ws, PWideChar(pw^.data), pw^.len);
                       constdef.VStr:=UTF8Encode(ws);
                       constdef.ConstType:=ctStr;
                     end
                   else if widecharsize=4 then
                     begin
                       for i:=0 to pw^.len-1 do
                         pw^.data[i]:=cardinal(ppufile.getlongint);
                     end
                   else
                     begin
                       WriteError('Unsupported tcompilerwidechar size');
                     end;
                   Write([space,'Wide string type']);
                   startnewline:=true;
                   for i:=0 to pw^.len-1 do
                     begin
                       if startnewline then
                         begin
                           writeln;
                           write(space);
                           startnewline:=false;
                         end;
                       ch:=pw^.data[i];
                       if widecharsize=2 then
                         write(hexstr(ch,4))
                       else
                         write(hexstr(ch,8));
                       if ((i + 1) mod 8)= 0 then
                         startnewline:=true
                       else
                         if i <> pw^.len-1 then
                           write(', ');
                     end;
                   donewidestring(pw);
                   Writeln;
                 end;
               constguid:
                 begin
                    getdata(guid,sizeof(guid));
                    write ([space,'     IID String: {',hexstr(guid.d1,8),'-',hexstr(guid.d2,4),'-',hexstr(guid.d3,4),'-']);
                    for i:=0 to 7 do
                      begin
                         write(hexstr(guid.d4[i],2));
                         if i=1 then write('-');
                      end;
                    writeln('}');
                 end
               else
                 Writeln (['!! Invalid unit format : Invalid const type encountered: ',b]);
             end;
           end;

         ibabsolutevarsym :
           begin
             def:=TPpuVarDef.Create(ParentDef);
             readabstractvarsym('Absolute variable symbol ',varoptions,TPpuVarDef(def));
             Write ([space,' Relocated to ']);
             b:=getbyte;
             case absolutetyp(b) of
               tovar :
                 readpropaccesslist(space+'          Sym : ');
               toasm :
                 Writeln(['Assembler name : ',getstring]);
               toaddr :
                 begin
                   Write(['Address : ',getaword]);
                   if tsystemcpu(ppufile.header.cpu)=cpu_i386 then
                     Write([' (Far: ',getbyte<>0,')']);
                   if tsystemcpu(ppufile.header.cpu)=cpu_i8086 then
                     if getbyte<>0 then
                       Write([' (Far: TRUE, Segment=',getaword,')'])
                     else
                       Write([' (Far: FALSE)']);
                   Writeln;
                 end;
               else
                 Writeln (['!! Invalid unit format : Invalid absolute type encountered: ',b]);
             end;
           end;

         ibfieldvarsym :
           begin
             def:=TPpuFieldDef.Create(ParentDef);
             readabstractvarsym('Field Variable symbol ',varoptions,TPpuVarDef(def));
             writeln([space,'      Address : ',getaint]);
           end;

         ibstaticvarsym :
           begin
             def:=TPpuVarDef.Create(ParentDef);
             readabstractvarsym('Global Variable symbol ',varoptions,TPpuVarDef(def));
             write  ([space,' DefaultConst : ']);
             readderef('');
             if (vo_has_mangledname in varoptions) then
{$ifdef symansistr}
               writeln([space,' Mangledname : ',getansistring]);
{$else symansistr}
               writeln([space,' Mangledname : ',getstring]);
{$endif symansistr}
           end;

         iblocalvarsym :
           begin
             readabstractvarsym('Local Variable symbol ',varoptions);
             write  ([space,' DefaultConst : ']);
             readderef('');
           end;

         ibparavarsym :
           begin
             def:=TPpuParamDef.Create(ParentDef);
             readabstractvarsym('Parameter Variable symbol ',varoptions,TPpuVarDef(def));
             write  ([space,' DefaultConst : ']);
             readderef('',TPpuParamDef(def).DefaultValue);
             writeln([space,'       ParaNr : ',getword]);
             writeln([space,'        Univ  : ',boolean(getbyte)]);
             writeln([space,'     VarState : ',getbyte]);
             writeln([space,'         Refs : ',getbyte]);
             if (vo_has_explicit_paraloc in varoptions) then
               begin
                 i:=getbyte;
                 getdata(tempbuf,i);
               end;
           end;

         ibenumsym :
           begin
             def:=TPpuConstDef.Create(nil);
             readcommonsym('Enumeration symbol ',def);
             write  ([space,'   Definition : ']);
             readderef('');
             TPpuConstDef(def).ConstType:=ctInt;
             TPpuConstDef(def).VInt:=getlongint;
             writeln([space,'        Value : ',TPpuConstDef(def).VInt]);
             if (ParentDef <> nil) and (ParentDef.DefType = dtEnum) then
               def.Parent:=ParentDef;
           end;

         ibsyssym :
           begin
             readcommonsym('Internal system symbol ');
             writeln([space,'  Internal Nr : ',getlongint]);
           end;

         ibmacrosym :
           begin
             readcommonsym('Macro symbol ');
             writeln([space,'          Name: ',getstring]);
             writeln([space,'       Defined: ',getbyte]);
             writeln([space,'  Compiler var: ',getbyte]);
             len:=getlongint;
             writeln([space,'  Value length: ',len]);
             if len > 0 then
               begin
                 getmem(pc,len+1);
                 getdata(pc^,len);
                 (pc+len)^:= #0;
                 writeln([space,'         Value: "',pc,'"']);
                 freemem(pc,len+1);
               end;
           end;

         ibpropertysym :
           begin
             def:=TPpuPropDef.Create(ParentDef);
             readcommonsym('Property ',def);
             propoptions:=readpropertyoptions;
             if ppo_overrides in propoptions then
               begin
                 write  ([space,' OverrideProp : ']);
                 readderef('');
               end;
             if ppo_defaultproperty in propoptions then
               Include(TPpuPropDef(def).Options, poDefault);
             write  ([space,'    Prop Type : ']);
             readderef('',TPpuPropDef(def).PropType);
             writeln([space,'        Index : ',getlongint]);
             writeln([space,'      Default : ',getlongint]);
             write  ([space,'   Index Type : ']);
             readderef('');
             { palt_none }
             readpropaccesslist('');
             write  ([space,'   Readaccess : ']);
             readpropaccesslist(space+'         Sym: ',TPpuPropDef(def).Getter);
             write  ([space,'  Writeaccess : ']);
             readpropaccesslist(space+'         Sym: ',TPpuPropDef(def).Setter);
             write  ([space,' Storedaccess : ']);
             readpropaccesslist(space+'         Sym: ');
             if [ppo_hasparameters,ppo_overrides]*propoptions=[ppo_hasparameters] then
               begin
                 space:='    '+space;
                 readsymtable('parast',TPpuPropDef(def));
                 delete(space,1,4);
               end;
           end;

         iberror :
           begin
             WriteError('!! Error in PPU');
             exit;
           end;

         ibendsyms :
           break;

         else
           begin
             WriteError('!! Skipping unsupported PPU Entry in Symbols: '+IntToStr(b));
           end;
       end;
       if (def <> nil) and (def.Parent = nil) then
         def.Free;
       if not EndOfEntry then
         HasMoreInfos;
     until false;
   end;
end;


{****************************************************************************
                         Read defintions Part
****************************************************************************}

procedure readdefinitions(const s:string; ParentDef: TPpuContainerDef);
{ type tordtype is in symconst unit }
{
    uvoid,
    u8bit,u16bit,u32bit,u64bit,
    s8bit,s16bit,s32bit,s64bit,
    bool8bit,bool16bit,bool32bit,bool64bit,
    uchar,uwidechar,scurrency
  ); }

{ type tobjecttyp is in symconst unit }
{ type tvarianttype is in symconst unit }
var
  b : byte;
  l,j : longint;
  calloption : tproccalloption;
  procoptions : tprocoptions;
  defoptions: tdefoptions;
  iexpr: Tconstexprint;
  def: TPpuDef;
  objdef: TPpuObjectDef absolute def;
  arrdef: TPpuArrayDef absolute def;
  enumdef: TPpuEnumDef absolute def;
  setdef: TPpuSetDef absolute def;
  orddef: TPpuOrdDef absolute def;
  floatdef: TPpuFloatDef absolute def;
  strdef: TPpuStringDef absolute def;
  filedef: TPpuFileDef absolute def;
begin
  with ppufile do
   begin
     if space<>'' then
      Writeln([space,'------ ',s,' ------']);
     if readentry<>ibstartdefs then
      Writeln('!! ibstartdefs not found');
     repeat
       def:=nil;
       b:=readentry;
       case b of

         ibpointerdef :
           begin
             def:=TPpuPointerDef.Create(ParentDef);
             readcommondef('Pointer definition',defoptions,def);
             write  ([space,'     Pointed Type : ']);
             readderef('',TPpuPointerDef(def).Ptr);
             if tsystemcpu(ppufile.header.cpu) in [cpu_i8086,cpu_i386,cpu_x86_64] then
               begin
                 write([space,' X86 Pointer Type : ']);
                 b:=getbyte;
                 case tx86pointertyp(b) of
                   x86pt_near: writeln('Near');
                   x86pt_near_cs: writeln('Near ''CS''');
                   x86pt_near_ds: writeln('Near ''DS''');
                   x86pt_near_ss: writeln('Near ''SS''');
                   x86pt_near_es: writeln('Near ''ES''');
                   x86pt_near_fs: writeln('Near ''FS''');
                   x86pt_near_gs: writeln('Near ''GS''');
                   x86pt_far: writeln('Far');
                   x86pt_huge: writeln('Huge');
                   else
                     WriteWarning('Invalid x86 pointer type: ' + IntToStr(b));
                 end;
               end;
             writeln([space,' Has Pointer Math : ',(getbyte<>0)]);
           end;

         iborddef :
           begin
             orddef:=TPpuOrdDef.Create(ParentDef);
             readcommondef('Ordinal definition',defoptions,orddef);
             write  ([space,'        Base type : ']);
             b:=getbyte;
             case tordtype(b) of
               uvoid:
                 begin
                   writeln('uvoid');
                   orddef.OrdType:=otVoid;
                 end;
               u8bit:
                 begin
                   writeln('u8bit');
                   orddef.OrdType:=otUInt;
                   orddef.Size:=1;
                 end;
               u16bit:
                 begin
                   writeln('u16bit');
                   orddef.OrdType:=otUInt;
                   orddef.Size:=2;
                 end;
               u32bit:
                 begin
                   writeln('u32bit');
                   orddef.OrdType:=otUInt;
                   orddef.Size:=4;
                 end;
               u64bit:
                 begin
                   writeln('u64bit');
                   orddef.OrdType:=otUInt;
                   orddef.Size:=8;
                 end;
               s8bit:
                 begin
                   writeln('s8bit');
                   orddef.OrdType:=otSInt;
                   orddef.Size:=1;
                 end;
               s16bit:
                 begin
                   writeln('s16bit');
                   orddef.OrdType:=otSInt;
                   orddef.Size:=2;
                 end;
               s32bit:
                 begin
                   writeln('s32bit');
                   orddef.OrdType:=otSInt;
                   orddef.Size:=4;
                 end;
               s64bit:
                 begin
                   writeln('s64bit');
                   orddef.OrdType:=otSInt;
                   orddef.Size:=8;
                 end;
               pasbool8:
                 begin
                   writeln('pasbool8');
                   orddef.OrdType:=otPasBool;
                   orddef.Size:=1;
                 end;
               pasbool16:
                 begin
                   writeln('pasbool16');
                   orddef.OrdType:=otPasBool;
                   orddef.Size:=2;
                 end;
               pasbool32:
                 begin
                   writeln('pasbool32');
                   orddef.OrdType:=otPasBool;
                   orddef.Size:=4;
                 end;
               pasbool64:
                 begin
                   writeln('pasbool64');
                   orddef.OrdType:=otPasBool;
                   orddef.Size:=8;
                 end;
               bool8bit:
                 begin
                   writeln('bool8bit');
                   orddef.OrdType:=otBool;
                   orddef.Size:=1;
                 end;
               bool16bit:
                 begin
                   writeln('bool16bit');
                   orddef.OrdType:=otBool;
                   orddef.Size:=2;
                 end;
               bool32bit:
                 begin
                   writeln('bool32bit');
                   orddef.OrdType:=otBool;
                   orddef.Size:=4;
                 end;
               bool64bit:
                 begin
                   writeln('bool64bit');
                   orddef.OrdType:=otBool;
                   orddef.Size:=8;
                 end;
               uchar:
                 begin
                   writeln('uchar');
                   orddef.OrdType:=otChar;
                   orddef.Size:=1;
                 end;
               uwidechar:
                 begin
                   writeln('uwidechar');
                   orddef.OrdType:=otChar;
                   orddef.Size:=2;
                 end;
               scurrency:
                 begin
                   writeln('scurrency');
                   orddef.OrdType:=otCurrency;
                   orddef.Size:=8;
                 end;
               else
                 WriteWarning('Invalid base type: ' + IntToStr(b));
             end;
             iexpr:=getexprint;
             orddef.RangeLow:=iexpr.svalue;
             write([space,'            Range : ',constexp.tostr(iexpr)]);
             iexpr:=getexprint;
             orddef.RangeHigh:=iexpr.svalue;
             writeln([' to ',constexp.tostr(iexpr)]);
           end;

         ibfloatdef :
           begin
             floatdef:=TPpuFloatDef.Create(ParentDef);
             readcommondef('Float definition',defoptions,floatdef);
             write  ([space,'       Float type : ']);
             b:=getbyte;
             case b of
               ftSingle:
                 begin
                   writeln('Single');
                   floatdef.FloatType:=pftSingle;
                 end;
               ftDouble:
                 begin
                   writeln('Double');
                   floatdef.FloatType:=pftDouble;
                 end;
               ftExtended:
                 begin
                   writeln('Extended');
                   floatdef.FloatType:=pftExtended;
                 end;
               ftComp:
                 begin
                   writeln('Comp');
                   floatdef.FloatType:=pftComp;
                 end;
               ftCurr:
                 begin
                   writeln('Currency');
                   floatdef.FloatType:=pftCurrency;
                 end;
               ftFloat128:
                 begin
                   writeln('Float128');
                   floatdef.FloatType:=pftFloat128;
                 end;
               else
                 WriteWarning('Invalid float type: ' + IntToStr(b));
             end;
           end;

         ibarraydef :
           begin
             arrdef:=TPpuArrayDef.Create(ParentDef);
             readcommondef('Array definition',defoptions,arrdef);
             write  ([space,'     Element type : ']);
             readderef('',arrdef.ElType);
             write  ([space,'       Range Type : ']);
             readderef('',arrdef.RangeType);
             arrdef.RangeLow:=getasizeint;
             arrdef.RangeHigh:=getasizeint;
             writeln([space,'            Range : ',arrdef.RangeLow,' to ',arrdef.RangeHigh]);
             write  ([space,'          Options : ']);
             readarraydefoptions(arrdef);
             readsymtable('symbols', arrdef);
           end;

         ibprocdef :
           begin
             def:=TPpuProcDef.Create(ParentDef);
             readcommondef('Procedure definition',defoptions,def);
             read_abstract_proc_def(calloption,procoptions,TPpuProcDef(def));
             if (po_has_mangledname in procoptions) then
{$ifdef symansistr}
               writeln([space,'     Mangled name : ',getansistring]);
{$else symansistr}
               writeln([space,'     Mangled name : ',getstring]);
{$endif symansistr}
             writeln([space,'           Number : ',getword]);
             writeln([space,'            Level : ',getbyte]);
             write  ([space,'            Class : ']);
             readderef('');
             write  ([space,'          Procsym : ']);
             readderef('', def.Ref);
             write  ([space,'         File Pos : ']);
             readposinfo(def);
             write  ([space,'       Visibility : ']);
             readvisibility(def);
             write  ([space,'       SymOptions : ']);
             readsymoptions(space+'       ');
             write  ([space,'   Synthetic kind : ',Synthetic2Str(ppufile.getbyte)]);
             if tsystemcpu(ppufile.header.cpu)=cpu_powerpc then
               begin
                 { library symbol for AmigaOS/MorphOS }
                 write  ([space,'   Library symbol : ']);
                 readderef('');
               end;
             if (po_has_importdll in procoptions) then
               writeln([space,'      Import DLL : ',getstring]);
             if (po_has_importname in procoptions) then
               writeln([space,'      Import Name : ',getstring]);
             writeln([space,'        Import Nr : ',getword]);
             if (po_msgint in procoptions) then
               writeln([space,'           MsgInt : ',getlongint]);
             if (po_msgstr in procoptions) then
               writeln([space,'           MsgStr : ',getstring]);
             if (po_dispid in procoptions) then
               writeln([space,'      DispID: ',ppufile.getlongint]);
             if (po_has_inlininginfo in procoptions) then
              begin
                write  ([space,'       FuncretSym : ']);
                readderef('');
                readprocinfooptions(space);
              end;
             b:=ppufile.getbyte;
             if b<>0 then
               begin
                 write  ([space,'       Alias names : ']);
                 for j:=1 to b do
                   begin
                     write(ppufile.getstring);
                     if j<b then
                       write(', ');
                   end;
                 writeln;
               end;
             writeln([space,'            Empty : ',getbyte<>0]);
             if not EndOfEntry then
               HasMoreInfos;
             space:='    '+space;
             { parast }
             readsymtable('parast', TPpuProcDef(def));
             { localst }
             if (po_has_inlininginfo in procoptions) then
                readsymtable('localst');
             if (po_has_inlininginfo in procoptions) then
               readnodetree;
             delete(space,1,4);
           end;

         ibprocvardef :
           begin
             def:=TPpuProcTypeDef.Create(ParentDef);
             readcommondef('Procedural type (ProcVar) definition',defoptions,def);
             read_abstract_proc_def(calloption,procoptions, TPpuProcDef(def));
             writeln([space,'   Symtable level :',ppufile.getbyte]);
             if not EndOfEntry then
               HasMoreInfos;
             space:='    '+space;
             { parast }
             readsymtable('parast',TPpuProcDef(def));
             delete(space,1,4);
           end;

         ibshortstringdef :
           begin
             strdef:=TPpuStringDef.Create(ParentDef);
             strdef.StrType:=stShort;
             readcommondef('ShortString definition',defoptions,strdef);
             strdef.Len:=getbyte;
             writeln([space,'           Length : ',strdef.Len]);
           end;

         ibwidestringdef :
           begin
             strdef:=TPpuStringDef.Create(ParentDef);
             strdef.StrType:=stWide;
             readcommondef('WideString definition',defoptions,strdef);
             strdef.Len:=getaint;
             writeln([space,'           Length : ',strdef.Len]);
           end;

         ibunicodestringdef :
           begin
             strdef:=TPpuStringDef.Create(ParentDef);
             strdef.StrType:=stUnicode;
             readcommondef('UnicodeString definition',defoptions,strdef);
             strdef.Len:=getaint;
             writeln([space,'           Length : ',strdef.Len]);
           end;

         ibansistringdef :
           begin
             strdef:=TPpuStringDef.Create(ParentDef);
             strdef.StrType:=stAnsi;
             readcommondef('AnsiString definition',defoptions,strdef);
             strdef.Len:=getaint;
             writeln([space,'           Length : ',strdef.Len]);
           end;

         iblongstringdef :
           begin
             strdef:=TPpuStringDef.Create(ParentDef);
             strdef.StrType:=stLong;
             readcommondef('Longstring definition',defoptions,strdef);
             strdef.Len:=getaint;
             writeln([space,'           Length : ',strdef.Len]);
           end;

         ibrecorddef :
           begin
             objdef:=TPpuRecordDef.Create(ParentDef);
             readcommondef('Record definition',defoptions, objdef);
             def.Name:=getstring;
             writeln([space,'   Name of Record : ',objdef.Name]);
             writeln([space,'   Import lib/pkg : ',getstring]);
             write  ([space,'          Options : ']);
             readobjectdefoptions(objdef);
             if (df_copied_def in defoptions) then
               begin
                 Include(TPpuRecordDef(def).Options, ooCopied);
                 write([space,'      Copied from : ']);
                 readderef('',objdef.Ancestor);
               end
             else
               begin
                 writeln([space,'       FieldAlign : ',shortint(getbyte)]);
                 writeln([space,'      RecordAlign : ',shortint(getbyte)]);
                 writeln([space,'         PadAlign : ',shortint(getbyte)]);
                 writeln([space,'UseFieldAlignment : ',shortint(getbyte)]);
                 objdef.Size:=getasizeint;
                 writeln([space,'         DataSize : ',objdef.Size]);
                 writeln([space,'      PaddingSize : ',getword]);
               end;
             if not EndOfEntry then
               HasMoreInfos;
             {read the record definitions and symbols}
             if not(df_copied_def in current_defoptions) then
               begin
                 space:='    '+space;
                 readrecsymtableoptions;
                 readsymtable('fields',TPpuRecordDef(def));
                 Delete(space,1,4);
               end;
           end;

         ibobjectdef :
           begin
             objdef:=TPpuObjectDef.Create(ParentDef);
             readcommondef('Object/Class definition',defoptions,objdef);
             objdef.Name:=getstring;
             writeln([space,'    Name of Class : ',objdef.Name]);
             writeln([space,'   Import lib/pkg : ',getstring]);
             write  ([space,'          Options : ']);
             readobjectdefoptions(objdef);
             b:=getbyte;
             write  ([space,'             Type : ']);
             case tobjecttyp(b) of
               odt_class          : writeln('class');
               odt_object         : writeln('object');
               odt_interfacecom   : writeln('interfacecom');
               odt_interfacecorba : writeln('interfacecorba');
               odt_cppclass       : writeln('cppclass');
               odt_dispinterface  : writeln('dispinterface');
               odt_objcclass      : writeln('objcclass');
               odt_objcprotocol   : writeln('objcprotocol');
               odt_helper         : writeln('helper');
               odt_objccategory   : writeln('objccategory');
               odt_javaclass      : writeln('Java class');
               odt_interfacejava  : writeln('Java interface');
               else                 WriteWarning('Invalid object type: ' + IntToStr(b));
             end;
             case tobjecttyp(b) of
               odt_class, odt_cppclass, odt_objcclass, odt_javaclass:
                 objdef.ObjType:=otClass;
               odt_object:
                 objdef.ObjType:=otObject;
               odt_interfacecom, odt_interfacecorba, odt_interfacejava, odt_dispinterface:
                 objdef.ObjType:=otInterface;
               odt_helper:
                 objdef.ObjType:=otHelper;
             end;
             writeln([space,'    External name : ',getstring]);
             objdef.Size:=getasizeint;
             writeln([space,'         DataSize : ',objdef.Size]);
             writeln([space,'      PaddingSize : ',getword]);
             writeln([space,'       FieldAlign : ',shortint(getbyte)]);
             writeln([space,'      RecordAlign : ',shortint(getbyte)]);
             writeln([space,'       Vmt offset : ',getlongint]);
             write  ([space,  '   Ancestor Class : ']);
             readderef('',objdef.Ancestor);

             if tobjecttyp(b) in [odt_interfacecom,odt_interfacecorba,odt_dispinterface] then
               begin
                  { IIDGUID }
                  for j:=1to 16 do
                   getbyte;
                  objdef.IID:=getstring;
                  writeln([space,'       IID String : ',objdef.IID]);
               end;

             writeln([space,' Abstract methods : ',getlongint]);

             if (tobjecttyp(b)=odt_helper) or
                 (oo_is_classhelper in current_objectoptions) then
               begin
                 write([space,'    Helper parent : ']);
                 readderef('',objdef.HelperParent);
               end;

             l:=getlongint;
             writeln([space,'  VMT entries: ',l]);
             for j:=1 to l do
               begin
                 write([space,'    ']);
                 readderef('');
                 write([space,'      Visibility: ']);
                 readvisibility;
               end;

             if tobjecttyp(b) in [odt_class,odt_objcclass,odt_objcprotocol,odt_javaclass,odt_interfacejava] then
              begin
                l:=getlongint;
                writeln([space,'  Impl Intf Count : ',l]);
                for j:=1 to l do
                 begin
                   write  ([space,'  - Definition : ']);
                   readderef('');
                   write  ([space,'  - Getter Def : ']);
                   readderef('');
                   writeln([space,'       IOffset : ',getlongint]);
                   writeln([space,'    Entry type : ',IntfEntryType2Str(getbyte)]);
                 end;
              end;

             if df_copied_def in current_defoptions then
               begin
                 Include(objdef.Options, ooCopied);
                 writeln('  Copy of def: ');
                 readderef('',objdef.Ancestor);
               end;

             if not EndOfEntry then
               HasMoreInfos;
             if not(df_copied_def in current_defoptions) then
               begin
                 {read the record definitions and symbols}
                 space:='    '+space;
                 readrecsymtableoptions;
                 readsymtable('fields',objdef);
                 Delete(space,1,4);
              end;
           end;

         ibfiledef :
           begin
             filedef:=TPpuFileDef.Create(ParentDef);
             ReadCommonDef('File definition',defoptions,filedef);
             write  ([space,'             Type : ']);
             case getbyte of
              0 : begin
                    writeln('Text');
                    filedef.FileType:=ftText;
                  end;
              1 : begin
                    writeln('Typed');
                    filedef.FileType:=ftTyped;
                    write  ([space,'      File of Type : ']);
                    readderef('',filedef.TypeRef);
                  end;
              2 : begin
                    writeln('Untyped');
                    filedef.FileType:=ftUntyped;
                  end;
             end;
           end;

         ibformaldef :
           begin
             def:=TPpuFormalDef.Create(ParentDef);
             readcommondef('Generic definition (void-typ)',defoptions,def);
             TPpuFormalDef(def).IsTyped:=(getbyte<>0);
             writeln([space,'         Is Typed : ',TPpuFormalDef(def).IsTyped]);
           end;

         ibundefineddef :
           begin
             def:=TPpuUndefinedDef.Create(ParentDef);
             readcommondef('Undefined definition (generic parameter)',defoptions,def);
           end;

         ibenumdef :
           begin
             enumdef:=TPpuEnumDef.Create(ParentDef);
             readcommondef('Enumeration type definition',defoptions,enumdef);
             enumdef.ElLow:=getaint;
             writeln([space,' Smallest element : ',enumdef.ElLow]);
             enumdef.ElHigh:=getaint;
             writeln([space,'  Largest element : ',enumdef.ElHigh]);
             enumdef.Size:=byte(getaint);
             writeln([space,'             Size : ',enumdef.Size]);
{$ifdef jvm}
             write([space,'        Class def : ']);
             readderef('');
{$endif}
             if df_copied_def in defoptions then
               begin
                 write([space,'Base enumeration type : ']);
                 readderef('',enumdef.CopyFrom);
               end
             else
               begin
                 space:='    '+space;
                 readsymtable('elements',enumdef);
                 delete(space,1,4);
               end;
           end;

         ibclassrefdef :
           begin
             def:=TPpuClassRefDef.Create(ParentDef);
             readcommondef('Class reference definition',defoptions,def);
             write  ([space,'    Pointed Type : ']);
             readderef('',TPpuClassRefDef(def).ClassRef);
           end;

         ibsetdef :
           begin
             setdef:=TPpuSetDef.Create(ParentDef);
             readcommondef('Set definition',defoptions,setdef);
             write  ([space,'     Element type : ']);
             readderef('',setdef.ElType);
             setdef.Size:=getaint;
             writeln([space,'             Size : ',setdef.Size]);
             setdef.SetBase:=getaint;
             writeln([space,'         Set Base : ',setdef.SetBase]);
             setdef.SetMax:=getaint;
             writeln([space,'          Set Max : ',setdef.SetMax]);
           end;

         ibvariantdef :
           begin
             def:=TPpuVariantDef.Create(ParentDef);
             readcommondef('Variant definition',defoptions,def);
             write  ([space,'      Varianttype : ']);
             b:=getbyte;
             case tvarianttype(b) of
               vt_normalvariant :
                 writeln('Normal');
               vt_olevariant :
                 begin
                   TPpuVariantDef(def).IsOLE:=True;
                   writeln('OLE');
                 end
               else
                 WriteWarning('Invalid varianttype: ' + IntToStr(b));
             end;
           end;

         iberror :
           begin
             WriteError('!! Error in PPU');
             exit;
           end;

         ibenddefs :
           break;

         else
           begin
             WriteError('!! Skipping unsupported PPU Entry in definitions: '+IntToStr(b));
           end;
       end;
       if (def <> nil) and (def.Parent = nil) then
         def.Free;
       if not EndOfEntry then
         HasMoreInfos;
     until false;
   end;
end;

procedure readmoduleoptions(space : string);
type
{ tmoduleoption type is in unit fmodule }
  tmoduleoption = (mo_none,
    mo_hint_deprecated,
    mo_hint_platform,
    mo_hint_library,
    mo_hint_unimplemented,
    mo_hint_experimental,
    mo_has_deprecated_msg
  );
  tmoduleoptions = set of tmoduleoption;
  tmoduleopt=record
    mask : tmoduleoption;
    str  : string[30];
  end;
const
  moduleopts=ord(high(tmoduleoption));
  moduleopt : array[1..moduleopts] of tmoduleopt=(
     (mask:mo_hint_deprecated;    str:'Hint Deprecated'),
     (mask:mo_hint_platform;      str:'Hint Platform'),
     (mask:mo_hint_library;       str:'Hint Library'),
     (mask:mo_hint_unimplemented; str:'Hint Unimplemented'),
     (mask:mo_hint_experimental;  str:'Hint Experimental'),
     (mask:mo_has_deprecated_msg; str:'Has Deprecated Message')
  );
var
  moduleoptions : tmoduleoptions;
  i      : longint;
  first  : boolean;
begin
  ppufile.getsmallset(moduleoptions);
  if moduleoptions<>[] then
   begin
     first:=true;
     for i:=1to moduleopts do
      if (moduleopt[i].mask in moduleoptions) then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(moduleopt[i].str);
       end;
   end;
  writeln;
  if mo_has_deprecated_msg in moduleoptions then
    writeln([space,'Deprecated : ', ppufile.getstring]);
end;

{****************************************************************************
                           Read General Part
****************************************************************************}

procedure readinterface;
var
  b : byte;
  sourcenumber, i : longint;
begin
  with ppufile do
   begin
     repeat
       b:=readentry;
       case b of

         ibmodulename :
           begin
             CurUnit.Name:=getstring;
             Writeln(['Module Name: ',CurUnit.Name]);
           end;

         ibmoduleoptions:
           readmoduleoptions('  ');

         ibsourcefiles :
           begin
             sourcenumber:=1;
             while not EndOfEntry do
              begin
                with TPpuSrcFile.Create(CurUnit.SourceFiles) do begin
                  Name:=getstring;
                  i:=getlongint;
                  if i >= 0 then
                    FileTime:=FileDateToDateTime(i);
                  Writeln(['Source file ',sourcenumber,' : ',Name,' ',filetimestring(i)]);
                end;

                inc(sourcenumber);
              end;
           end;
{$IFDEF MACRO_DIFF_HINT}
         ibusedmacros :
           begin
             while not EndOfEntry do
              begin
                Write('Conditional ',getstring);
                b:=getbyte;
                if boolean(b)=true then
                  write(' defined at startup')
                else
                  write(' not defined at startup');
                b:=getbyte;
                if boolean(b)=true then
                  writeln(' was used')
                else
                  writeln;
              end;
           end;
{$ENDIF}
         ibloadunit :
           ReadLoadUnit;

         iblinkunitofiles :
           ReadLinkContainer('Link unit object file: ');

         iblinkunitstaticlibs :
           ReadLinkContainer('Link unit static lib: ');

         iblinkunitsharedlibs :
           ReadLinkContainer('Link unit shared lib: ');

         iblinkotherofiles :
           ReadLinkContainer('Link other object file: ');

         iblinkotherstaticlibs :
           ReadLinkContainer('Link other static lib: ');

         iblinkothersharedlibs :
           ReadLinkContainer('Link other shared lib: ');

         iblinkotherframeworks:
           ReadLinkContainer('Link framework: ');

         ibmainname:
           Writeln(['Specified main program symbol name: ',getstring]);

         ibImportSymbols :
           ReadImportSymbols;

         ibderefdata :
           ReadDerefData;

         ibderefmap :
           ReadDerefMap;

         ibwpofile :
           ReadWpoFileInfo;

         ibresources :
           ReadLinkContainer('Resource file: ');

         iberror :
           begin
             WriteError('Error in PPU');
             exit;
           end;

         ibendinterface :
           break;

         else
           begin
             WriteError('!! Skipping unsupported PPU Entry in General Part: '+IntToStr(b));
           end;
       end;
     until false;
   end;
end;



{****************************************************************************
                        Read Implementation Part
****************************************************************************}

procedure readimplementation;
var
  b : byte;
begin
  with ppufile do
   begin
     repeat
       b:=readentry;
       case b of
         ibasmsymbols :
           ReadAsmSymbols;

         ibloadunit :
           ReadLoadUnit;

         iberror :
           begin
             WriteError('Error in PPU');
             exit;
           end;
         ibendimplementation :
           break;
         else
           begin
             WriteError('!! Skipping unsupported PPU Entry in Implementation: '+IntToStr(b));
           end;
       end;
     until false;
   end;
end;


procedure dofile (filename : string);
begin
{ reset }
  space:='';
{ fix filename }
  if pos('.',filename)=0 then
   filename:=filename+'.ppu';
  ppufile:=tppufile.create(filename);
  if not ppufile.openfile then
   begin
     WriteError('IO-Error when opening : '+filename+', Skipping');
     exit;
   end;
{ PPU File is open, check for PPU Id }
  if not ppufile.CheckPPUID then
   begin
     WriteError(Filename+' : Not a valid PPU file, Skipping');
     exit;
   end;
{ Check PPU Version }
  ppuversion:=ppufile.GetPPUVersion;

  Writeln(['Analyzing ',filename,' (v',PPUVersion,')']);
  if PPUVersion<16 then
   begin
     WriteError(Filename+' : Old PPU Formats (<v16) are not supported, Skipping');
     exit;
   end;

  if not SkipVersionCheck and (PPUVersion <> CurrentPPUVersion) then
   begin
     WriteError(Format('Unsupported PPU version %d. Expecting PPU version %d.', [PPUVersion, CurrentPPUVersion]));
     exit;
   end;

  CurUnit:=TPpuUnitDef.Create(UnitList);
  CurUnit.Version:=ppuversion;

{ Write PPU Header Information }
  if (verbose and v_header)<>0 then
   begin
     Writeln;
     Writeln('Header');
     Writeln('-------');
     with ppufile.header do
      begin
        Writeln(['Compiler version        : ',ppufile.header.compiler shr 14,'.',
                                             (ppufile.header.compiler shr 7) and $7f,'.',
                                             ppufile.header.compiler and $7f]);
        WriteLn(['Target processor        : ',Cpu2Str(cpu)]);
        WriteLn(['Target operating system : ',Target2Str(target)]);
        Writeln(['Unit flags              : ',PPUFlags2Str(flags)]);
        Writeln(['FileSize (w/o header)   : ',size]);
        Writeln(['Checksum                : ',hexstr(checksum,8)]);
        Writeln(['Interface Checksum      : ',hexstr(interface_checksum,8)]);
        Writeln(['Indirect Checksum       : ',hexstr(indirect_checksum,8)]);
        Writeln(['Definitions stored      : ',tostr(deflistsize)]);
        Writeln(['Symbols stored          : ',tostr(symlistsize)]);
      end;
   end;

  with ppufile.header do
    begin
      CurUnit.Crc:=checksum;
      CurUnit.IntfCrc:=interface_checksum;
      CurUnit.TargetCPU:=Cpu2Str(cpu);
      CurUnit.TargetOS:=Target2Str(target);
    end;

{read the general stuff}
  if (verbose and v_interface)<>0 then
   begin
     Writeln;
     Writeln('Interface section');
     Writeln('------------------');
     readinterface;
   end
  else
   ppufile.skipuntilentry(ibendinterface);
  Writeln;
  Writeln('Interface symtable');
  Writeln('----------------------');
  readsymtableoptions('interface');
{read the definitions}
  if (verbose and v_defs)<>0 then
   begin
     Writeln;
     Writeln('Interface definitions');
     Writeln('----------------------');
     readdefinitions('interface', CurUnit);
   end
  else
   ppufile.skipuntilentry(ibenddefs);
{read the symbols}
  if (verbose and v_syms)<>0 then
   begin
     Writeln;
     Writeln('Interface Symbols');
     Writeln('------------------');
     readsymbols('interface',CurUnit);
   end
  else
   ppufile.skipuntilentry(ibendsyms);

{read the macro symbols}
  if (verbose and v_syms)<>0 then
   begin
     Writeln;
     Writeln('Interface Macro Symbols');
     Writeln('-----------------------');
   end;
  if ppufile.readentry<>ibexportedmacros then
    begin
      WriteError('!! Error in PPU');
      exit;
    end;
  if boolean(ppufile.getbyte) then
    begin
      readsymtableoptions('interface macro');
      {skip the definition section for macros (since they are never used) }
      ppufile.skipuntilentry(ibenddefs);
      {read the macro symbols}
      if (verbose and v_syms)<>0 then
        readsymbols('interface macro')
      else
        ppufile.skipuntilentry(ibendsyms);
    end
  else
    Writeln('(no exported macros)');

{read the implementation stuff}
  if (verbose and v_implementation)<>0 then
   begin
     Writeln;
     Writeln('Implementation section');
     Writeln('-----------------------');
     readimplementation;
   end
  else
   ppufile.skipuntilentry(ibendimplementation);
  {read the static symtable}
  Writeln;
  Writeln('Implementation symtable');
  Writeln('----------------------');
  readsymtableoptions('implementation');
  if (ppufile.header.flags and uf_local_symtable)<>0 then
   begin
     if (verbose and v_defs)<>0 then
      begin
        Writeln;
        Writeln('Static definitions');
        Writeln('----------------------');
        readdefinitions('implementation', nil);
      end
     else
      ppufile.skipuntilentry(ibenddefs);
   {read the symbols}
     if (verbose and v_syms)<>0 then
      begin
        Writeln;
        Writeln('Static Symbols');
        Writeln('------------------');
        readsymbols('implementation');
      end
     else
      ppufile.skipuntilentry(ibendsyms);
   end;
  ReadCreatedObjTypes;
  FreeDerefdata;
{shutdown ppufile}
  ppufile.closefile;
  ppufile.free;
  Writeln;
end;

procedure WriteLogo;
begin
  writeln(Title+' Version '+version_string);
  writeln(Copyright);
  writeln;
end;

procedure help;
begin
  WriteLogo;
  writeln('usage: ppudump [options] <filename1> <filename2>...');
  writeln;
  writeln('[options] can be:');
  writeln('    -F<format>  Set output format to <format>');
  writeln('                  t - text format (default)');
  writeln('                  j - JSON format');
  writeln('                  x - XML format');
  writeln('    -M Exit with ExitCode=2 if more information is available');
  writeln('    -S Skip PPU version check. May lead to reading errors');
  writeln('    -V<verbose>  Set verbosity to <verbose>');
  writeln('                   H - Show header info');
  writeln('                   I - Show interface');
  writeln('                   M - Show implementation');
  writeln('                   S - Show interface symbols');
  writeln('                   D - Show interface definitions');
  writeln('                   A - Show all');
  writeln('    -h, -?       This helpscreen');
  halt;
end;

var
  startpara,
  nrfile,i  : longint;
  para      : string;
const
  error_on_more : boolean = false;
begin
  if paramcount<1 then
    help;
{ turn verbose on by default }
  verbose:=v_all;
{ read options }
  startpara:=1;
  while copy(paramstr(startpara),1,1)='-' do
   begin
     para:=paramstr(startpara);
     case upcase(para[2]) of
      'F' : begin
              FreeAndNil(pout);
              if Length(para) > 2 then
                case upcase(para[3]) of
                  'T':
                    nostdout:=False;
                  'J':
                    begin
                      nostdout:=True;
                      pout:=TPpuJsonOutput.Create(Output);
                    end;
                  'X':
                    begin
                      nostdout:=True;
                      pout:=TPpuXmlOutput.Create(Output);
                    end;
                  else
                    begin
                      WriteError('Invalid output format: ' + para[3]);
                      Halt(1);
                    end;
                end;
            end;
      'M' : error_on_more:=true;
      'S' : SkipVersionCheck:=True;
      'V' : begin
              verbose:=0;
              for i:=3 to length(para) do
               case upcase(para[i]) of
                'H' : verbose:=verbose or v_header;
                'I' : verbose:=verbose or v_interface;
                'M' : verbose:=verbose or v_implementation;
                'D' : verbose:=verbose or v_defs;
                'S' : verbose:=verbose or v_syms;
                'A' : verbose:=verbose or v_all;
               end;
            end;
      'H' : help;
      '?' : help;
      else
        begin
          WriteError('Invalid option: ' + para);
          Halt(1);
        end;
     end;
     inc(startpara);
   end;

  if not nostdout then
    WriteLogo;

  UnitList:=TPpuContainerDef.Create(nil);
  try
    UnitList.ItemsName:='';
    { process files }
    for nrfile:=startpara to paramcount do
      dofile (paramstr(nrfile));
    if not has_errors and (pout <> nil) then
     begin
       pout.Init;
       UnitList.Write(pout);
       pout.Done;
     end;
  finally
    UnitList.Free;
    pout.Free;
  end;
  if has_errors then
    Halt(1);
  if error_on_more and has_more_infos then
    Halt(2);
end.

