{
    $Id$
    Copyright (c) 1995-98 by the FPC Development Team

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
{$ifdef TP}
  {$N+,E+}
{$endif}
program pppdump;
uses ppu;

const
  Version   = 'Version 0.99.12';
  Title     = 'PPU-Analyser';
  Copyright = 'Copyright (c) 1995-99 by the Free Pascal Development Team';

{ verbosity }
  v_none           = $0;
  v_header         = $1;
  v_defs           = $2;
  v_syms           = $4;
  v_interface      = $8;
  v_implementation = $10;
  v_browser        = $20;
  v_all            = $ff;

var
  ppufile     : pppufile;
  space       : string;
  symcnt,
  defcnt      : longint;
  read_member : boolean;
  verbose     : longint;

{****************************************************************************
                          Helper Routines
****************************************************************************}

const has_errors : boolean = false;
Procedure Error(const S : string);
Begin
   Writeln(S);
   has_errors:=true;
End;

Function Target2Str(w:longint):string;
type
  ttarget = (target_none
     ,target_i386_GO32V1,target_i386_GO32V2,target_i386_linux,
      target_i386_OS2,target_i386_Win32
     ,target_m68k_Amiga,target_m68k_Atari,target_m68k_Mac,
      target_m68k_linux,target_m68k_PalmOS
  );
const
  Targets : array[ttarget] of string[10]=('none',
     'GO32V1','GO32V2','Linux-i386','OS/2','Win32',
     'Amiga','Mac68k','Atari','Linux-m68k','PalmOs');
begin
  if w<=ord(high(ttarget)) then
    Target2Str:=Targets[ttarget(w)]
  else
    Target2Str:='<Unknown>';
end;


Function Cpu2Str(w:longint):string;
type
  ttargetcpu=(no_cpu
       ,i386,m68k,alpha
  );
const
  CpuTxt : array[ttargetcpu] of string[5]=
    ('none','i386','m68k','alpha');
begin
  if w<=ord(high(ttargetcpu)) then
    Cpu2Str:=CpuTxt[ttargetcpu(w)]
  else
    Cpu2Str:='<Unknown>';
end;


const
  HexTbl : array[0..15] of char='0123456789ABCDEF';
function HexB(b:byte):string;
begin
  HexB[0]:=#2;
  HexB[1]:=HexTbl[b shr 4];
  HexB[2]:=HexTbl[b and $f];
end;


{****************************************************************************
                             Read Routines
****************************************************************************}

Procedure ReadContainer(const prefix:string);
{
  Read a serie of strings and write to the screen starting every line
  with prefix
}
begin
  while not ppufile^.endofentry do
   WriteLn(prefix,ppufile^.getstring);
end;


Procedure ReadRef;
begin
  if (verbose and v_browser)=0 then
   exit;
  while (not ppufile^.endofentry) and (not ppufile^.error) do
   Writeln(space,'        - Refered : ',ppufile^.getword,', (',ppufile^.getlongint,',',ppufile^.getword,')');
end;


Procedure ReadPosInfo;
begin
  Writeln(ppufile^.getword,' (',ppufile^.getlongint,',',ppufile^.getword,')');
end;


procedure readderef(const s:string);
type
  tdereftype = (derefnil,derefaktrecordindex,derefaktstaticindex,derefunit,derefrecord,derefindex);
var
  b : tdereftype;
begin
  repeat
    b:=tdereftype(ppufile^.getbyte);
    case b of
      derefnil :
        begin
          writeln('nil');
          break;
        end;
      derefaktrecordindex :
        begin
          writeln('AktRecord ',s,' ',ppufile^.getword);
          break;
        end;
      derefaktstaticindex :
        begin
          writeln('AktStatic ',s,' ',ppufile^.getword);
          break;
        end;
      derefunit :
        begin
          writeln('Unit ',ppufile^.getword);
          break;
        end;
      derefrecord :
        begin
          write('RecordDef ',ppufile^.getword,', ');
        end;
      derefindex :
        begin
          write(s,' ',ppufile^.getword,', ');
        end;
    end;
  until false;
end;

procedure readdefref;
begin
  readderef('Definition');
end;

procedure readsymref;
begin
  readderef('Symbol');
end;


procedure read_abstract_proc_def;
type
  tprocopt=record
    mask : longint;
    str  : string[30];
  end;
const
  procopts=24;
  procopt : array[1..procopts] of tprocopt=(
     (mask:1;        str:'Exception handler'),
     (mask:2;        str:'Virtual Method'),
     (mask:4;        str:'Stack is not cleared'),
     (mask:8;        str:'Constructor'),
     (mask:$10;      str:'Destructor'),
     (mask:$20;      str:'Internal Procedure'),
     (mask:$40;      str:'Exported Procedure'),
     (mask:$80;      str:'I/O-Checking'),
     (mask:$100;     str:'Abstract method'),
     (mask:$200;     str:'Interrupt Handler'),
     (mask:$400;     str:'Inline Procedure'),
     (mask:$800;     str:'Assembler Procedure'),
     (mask:$1000;    str:'Overloaded Operator'),
     (mask:$2000;    str:'External Procedure'),
     (mask:$4000;    str:'Parameters from left to right'),
     (mask:$8000;    str:'Main Program'),
     (mask:$10000;   str:'Static Method'),
     (mask:$20000;   str:'Method with Override Directive'),
     (mask:$40000;   str:'Class Method'),
     (mask:$80000;   str:'Unit Initialisation'),
     (mask:$100000;  str:'Method Pointer'),
     (mask:$200000;  str:'C Declaration'),
     (mask:$400000;  str:'PalmOS Syscall'),
     (mask:$800000;  str:'Has internal Constant Function')
  );
  tvarspez : array[0..2] of string[5]=('Value','Const','Var  ');
var
  procoptions,
  i,params : longint;
  first  : boolean;
begin
  write(space,'      Return type : ');
  readdefref;
  writeln(space,'         Fpu used : ',ppufile^.getbyte);
  procoptions:=ppufile^.getlongint;
  if procoptions<>0 then
   begin
     write(space,'          Options : ');
     first:=true;
     for i:=1to procopts do
      if (procoptions and procopt[i].mask)<>0 then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(procopt[i].str);
       end;
     writeln;
   end;
  params:=ppufile^.getword;
  writeln(space,' Nr of parameters : ',params);
  if params>0 then
   begin
     repeat
       write(space,'       - ',tvarspez[ppufile^.getbyte],' ');
       readdefref;
       dec(params);
     until params=0;
   end;
end;


procedure readcommonsym(const s:string);
type
  tsymopt=record
    mask : longint;
    str  : string[30];
  end;
const
  symopts=6;
  symopt : array[1..symopts] of tsymopt=(
     (mask:1;        str:'Public'),
     (mask:2;        str:'Private'),
     (mask:4;        str:'Published'),
     (mask:8;        str:'Protected'),
     (mask:$10;      str:'ForwardDef'),
     (mask:$20;      str:'Static')
  );
var
  symoptions,
  i      : longint;
  first  : boolean;
begin
  writeln(space,'** Symbol Nr. ',ppufile^.getword,' **');
  writeln(space,s,ppufile^.getstring);
  symoptions:=ppufile^.getbyte;
  if symoptions<>0 then
   begin
     write(space,'    File Pos: ');
     readposinfo;
     write(space,'     Options: ');
     first:=true;
     for i:=1to symopts do
      if (symoptions and symopt[i].mask)<>0 then
       begin
         if first then
           first:=false
         else
           write(', ');
         write(symopt[i].str);
       end;
     writeln;
   end;
end;


procedure readcommondef(const s:string);
begin
  writeln(space,'** Definition Nr. ',ppufile^.getword,' **');
  writeln(space,s);
  write  (space,'      Type symbol : ');
  readsymref;
end;


{****************************************************************************
                             Read Symbols Part
****************************************************************************}

procedure readsymbols;
Const
  vo_is_C_var = 2;
Type
  absolutetyp = (tovar,toasm,toaddr);
  tconsttype  = (constord,conststring,constreal,constbool,constint,constchar,constseta);
var
  b      : byte;
  totalsyms,
  symcnt,
  i,j    : longint;
begin
  symcnt:=1;
  with ppufile^ do
   begin
     if space<>'' then
      Writeln(space,'-----------------------------');
     if readentry=ibstartsyms then
      begin
        totalsyms:=getlongint;
        Writeln(space,'Number of symbols: ',totalsyms);
        Writeln(space,'Symtable datasize: ',getlongint);
      end
     else
      begin
        totalsyms:=-1;
        Writeln('!! ibstartsym not found');
      end;
     repeat
       b:=readentry;
       if not (b in [iberror,ibendsyms]) then
        inc(symcnt);
       case b of

         ibunitsym :
           readcommonsym('Unit symbol ');

         iblabelsym :
           readcommonsym('Label symbol ');

         ibtypesym :
           begin
             readcommonsym('Type symbol ');
             write(space,'  Definition: ');
             readdefref;
           end;

         ibprocsym :
           begin
             readcommonsym('Procedure symbol ');
             write(space,'  Definition: ');
             readdefref;
           end;

         ibconstsym :
           begin
             readcommonsym('Constant symbol ');
             b:=getbyte;
             case tconsttype(b) of
               constord :
                 begin
                   write (space,'  Definition : ');
                   readdefref;
                   writeln (space,'  Value : ',getlongint)
                 end;
               conststring :
                 writeln(space,'  Value : "'+getstring+'"');
               constreal :
                 writeln(space,'  Value : ',getreal);
               constbool :
                 if getlongint<>0 then
                   writeln (space,'  Value : True')
                 else
                   writeln (space,'  Value : False');
               constint :
                 writeln(space,'  Value : ',getlongint);
               constchar :
                 writeln(space,'  Value : "'+chr(getlongint)+'"');
               constseta :
                 begin
                   write (space,'  Definition : ');
                   readdefref;
                   for i:=1to 4 do
                    begin
                      write (space,'  Value : ');
                      for j:=1to 8 do
                       begin
                         if j>1 then
                          write(',');
                         write(hexb(getbyte));
                       end;
                      writeln;
                    end;
                 end;
               else
                 Writeln ('!! Invalid unit format : Invalid const type encountered: ',b);
             end;
           end;

         ibvarsym :
           begin
             readcommonsym('Variable symbol ');
             writeln(space,'        Type: ',getbyte);
             if read_member then
               writeln(space,'     Address: ',getlongint);
             write  (space,'  Definition: ');
             readdefref;
             i:=getbyte;
             writeln(space,'     Options: ',i);
             if (i and vo_is_C_var)<>0 then
              writeln(space,' Mangledname: ',getstring);
           end;

         ibenumsym :
           begin
             readcommonsym('Enumeration symbol ');
             write  (space,'  Definition: ');
             readdefref;
             writeln(space,'       Value: ',getlongint);
           end;

         ibsyssym :
           begin
             readcommonsym('Internal system symbol ');
             writeln(space,' Internal Nr: ',getlongint);
           end;

         ibtypedconstsym :
           begin
             readcommonsym('Typed constant ');
             write  (space,'  Definition: ');
             readdefref;
             writeln(space,'       Label: ',getstring);
           end;

         ibabsolutesym :
           begin
             readcommonsym('Absolute variable symbol ');
             writeln(space,'        Type: ',getbyte);
             if read_member then
               writeln(space,'     Address: ',getlongint);
             write  (space,'  Definition: ');
             readdefref;
             writeln(space,'     Options: ',getbyte);
             Write (space,'  Relocated to ');
             b:=getbyte;
             case absolutetyp(b) of
               tovar :
                 Writeln('Name : ',getstring);
               toasm :
                 Writeln('Assembler name : ',getstring);
               toaddr :
                 begin
                   Write('Address : ',getlongint);
                   write(' (Far: ',getbyte<>0,')');
                 end;
               else
                 Writeln ('!! Invalid unit format : Invalid absolute type encountered: ',b);
             end;
           end;

         ibpropertysym :
           begin
             readcommonsym('Property ');
             write  (space,'  Definition: ');
             readdefref;
             writeln(space,'     Options: ',getlongint);
             writeln(space,'       Index: ',getlongint);
             writeln(space,'     Default: ',getlongint);
             writeln(space,'   Read Name: ',getstring);
             writeln(space,'  Write Name: ',getstring);
             writeln(space,' Stored Name: ',getstring);
             write(space,'  Read Definition: ');
             readdefref;
             write(space,' Write Definition: ');
             readdefref;
             write(space,'Stored Definition: ');
             readdefref;
           end;

         ibfuncretsym :
           begin
             readcommonsym('Func return value ');
             write  (space,'  Definition: ');
             readdefref;
             writeln(space,'  Address: ',getlongint);
           end;

         iberror :
           begin
             Writeln('!! Error in PPU');
             exit;
           end;

         ibendsyms :
           break;

         else
           WriteLn('!! Skipping unsupported PPU Entry in Symbols: ',b);
       end;
       if not EndOfEntry then
        Writeln('!! Entry has more information stored');
     until false;
     if (totalsyms<>-1) and (symcnt-1<>totalsyms) then
       Writeln('!! Only read ',symcnt-1,' of ',totalsyms,' symbols');
   end;
end;


{****************************************************************************
                         Read defintions Part
****************************************************************************}

procedure readdefinitions(start_read : boolean);
type
  tsettype  = (normset,smallset,varset);
  tbasetype = (uauto,uvoid,uchar,
               u8bit,u16bit,u32bit,
               s8bit,s16bit,s32bit,
               bool8bit,bool16bit,bool32bit);
var
  b : byte;
  oldread_member : boolean;
  totaldefs,
  defcnt : longint;
begin
  defcnt:=0;
  with ppufile^ do
   begin
     if space<>'' then
      Writeln(space,'-----------------------------');
     if not start_read then
       if readentry=ibstartdefs then
         begin
           totaldefs:=getlongint;
           Writeln(space,'Number of definitions: ',totaldefs);
         end
       else
         begin
           totaldefs:=-1;
           Writeln('!! ibstartdef not found');
         end;
     repeat
       b:=readentry;
       if not (b in [iberror,ibenddefs]) then
        inc(defcnt);
       case b of

         ibpointerdef :
           begin
             readcommondef('Pointer definition');
             write  (space,'    To Definition : ');
             readdefref;
             writeln(space,'           Is Far : ',(getbyte<>0));
           end;

         iborddef :
           begin
             readcommondef('Ordinal definition');
             write  (space,'        Base type : ');
             b:=getbyte;
             case tbasetype(b) of
               uauto     : writeln('uauto');
               uvoid     : writeln('uvoid');
               uchar     : writeln('uchar');
               u8bit     : writeln('u8bit');
               u16bit    : writeln('u16bit');
               u32bit    : writeln('s32bit');
               s8bit     : writeln('s8bit');
               s16bit    : writeln('s16bit');
               s32bit    : writeln('s32bit');
               bool8bit  : writeln('bool8bit');
               bool16bit : writeln('bool16bit');
               bool32bit : writeln('bool32bit');
               else        writeln('!! Warning: Invalid base type ',b);
             end;
             writeln(space,'            Range : ',getlongint,' to ',getlongint);
           end;

         ibfloatdef :
           begin
             readcommondef('Float definition');
             writeln(space,'       Float type : ',getbyte);
           end;

         ibarraydef :
           begin
             readcommondef('Array definition');
             write  (space,'     Element type : ');
             readdefref;
             write  (space,'       Range Type : ');
             readdefref;
             writeln(space,'            Range : ',getlongint,' to ',getlongint);
             writeln(space,'   Is Constructor : ',(getbyte<>0));
           end;

         ibprocdef :
           begin
             readcommondef('Procedure definition');
             read_abstract_proc_def;
             writeln(space,'    Used Register : ',getbyte);
             writeln(space,'     Mangled name : ',getstring);
             writeln(space,'           Number : ',getlongint);
             write  (space,'             Next : ');
             readdefref;
             write  (space,'            Class : ');
             readdefref;
             write  (space,'         File Pos : ');
             readposinfo;
           end;

         ibprocvardef :
           begin
             readcommondef('Procedural type (ProcVar) definition');
             read_abstract_proc_def;
           end;

         ibshortstringdef :
           begin
             readcommondef('ShortString definition');
             writeln(space,'           Length : ',getbyte);
           end;

         ibwidestringdef :
           begin
             readcommondef('WideString definition');
             writeln(space,'           Length : ',getlongint);
           end;

         ibansistringdef :
           begin
             readcommondef('AnsiString definition');
             writeln(space,'           Length : ',getlongint);
           end;

         iblongstringdef :
           begin
             readcommondef('Longstring definition');
             writeln(space,'           Length : ',getlongint);
           end;

         ibrecorddef :
           begin
             readcommondef('Record definition');
             writeln(space,'             Size : ',getlongint);
             {read the record definitions and symbols}
             space:='    '+space;
             oldread_member:=read_member;
             read_member:=true;
             readdefinitions(false);
             readsymbols;
             read_member:=oldread_member;
             Delete(space,1,4);
           end;

         ibobjectdef :
           begin
             readcommondef('Object/Class definition');
             writeln(space,'             Size : ',getlongint);
             writeln(space,'       Vmt offset : ',getlongint);
             writeln(space,'    Name of Class : ',getstring);
             write(space,  '   Ancestor Class : ');
             readdefref;
             writeln(space,'          Options : ',getlongint);
           {read the record definitions and symbols}
             space:='    '+space;
             oldread_member:=read_member;
             read_member:=true;
             readdefinitions(false);
             readsymbols;
             read_member:=oldread_member;
             Delete(space,1,4);
           end;

         ibfiledef :
           begin
             ReadCommonDef('File definition');
             write  (space,'             Type : ');
             case getbyte of
              0 : writeln('Text');
              1 : begin
                    write('Typed with definition ');
                    readdefref;
                  end;
              2 : writeln('Untyped');
             end;
           end;

         ibformaldef :
           readcommondef('Generic Definition (void-typ)');

         ibenumdef :
           begin
             readcommondef('Enumeration type definition');
             write(space,'Base enumeration type : ');
             readdefref;
             writeln(space,' Smallest element : ',getlongint);
             writeln(space,'  Largest element : ',getlongint);
             writeln(space,'             Size : ',getlongint);
           end;

         ibclassrefdef :
           begin
             readcommondef('Class reference definition');
             write  (space,'    To definition : ');
             readdefref;
           end;

         ibsetdef :
           begin
             readcommondef('Set definition');
             write  (space,'     Element type : ');
             readdefref;
             b:=getbyte;
             case tsettype(b) of
               smallset : writeln(space,'  Set with 32 Elements');
               normset  : writeln(space,'  Set with 256 Elements');
               varset   : writeln(space,'  Set with ',getlongint,' Elements');
               else       writeln('!! Warning: Invalid set type ',b);
             end;
           end;

         iberror :
           begin
             Writeln('!! Error in PPU');
             exit;
           end;

         ibenddefs :
           break;

         else
           WriteLn('!! Skipping unsupported PPU Entry in definitions: ',b);
       end;
       if not EndOfEntry then
        Writeln('!! Entry has more information stored');
     until false;
     if (totaldefs<>-1) and (defcnt<>totaldefs) then
      Writeln('!! Only read ',defcnt,' of ',totaldefs,' definitions');
   end;
end;


{****************************************************************************
                           Read General Part
****************************************************************************}

procedure readinterface;
var
  b : byte;
  sourcenumber,
  unitnumber : word;
  ucrc,uintfcrc : longint;
begin
  with ppufile^ do
   begin
     repeat
       b:=readentry;
       case b of

         ibmodulename :
           Writeln('Module Name: ',getstring);

         ibsourcefiles :
           begin
             sourcenumber:=1;
             while not EndOfEntry do
              begin
                Writeln('Source file ',sourcenumber,' : ',getstring);
                inc(sourcenumber);
              end;
           end;

         ibloadunit :
           begin
             unitnumber:=1;
             while not EndOfEntry do
              begin
                write('Uses unit: ',getstring,' (Number: ',unitnumber,')');
                ucrc:=getlongint;
                uintfcrc:=getlongint;
                write(' (Crc: ',ucrc,', IntfcCrc: ',uintfcrc,')');
                if getbyte<>0 then
                 writeln(' (interface)')
                else
                 writeln(' (implementation)');
                inc(unitnumber);
              end;
           end;

         iblinkofiles :
           ReadContainer('Link object file: ');

         iblinkstaticlibs :
           ReadContainer('Link static lib: ');

         iblinksharedlibs :
           ReadContainer('Link shared lib: ');

         iblinkunitfiles :
           ReadContainer('Link unit file: ');

         iberror :
           begin
             Writeln('Error in PPU');
             exit;
           end;

         ibendinterface :
           break;

         else
           WriteLn('!! Skipping unsupported PPU Entry in General Part: ',b);
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
  with ppufile^ do
   begin
     repeat
       b:=readentry;
       case b of
         iberror :
           begin
             Writeln('Error in PPU');
             exit;
           end;
         ibendimplementation :
           break;
         else
           WriteLn('!! Skipping unsupported PPU Entry in Implementation: ',b);
       end;
     until false;
   end;
end;


{****************************************************************************
                            Read Browser Part
****************************************************************************}

procedure readbrowser;
var
  b : byte;
const indent : string = '';
begin
  Writeln(indent,'Start of symtable browser');
  indent:=indent+'**';
  with ppufile^ do
   begin
     repeat
       b:=readentry;
       case b of
        ibbeginsymtablebrowser :
                         { here we must read object and record symtables !! }
                    begin
                      indent:=indent+'  ';
                      Writeln(indent,'Record/Object symtable');
                      readbrowser;
                      Indent:=Copy(Indent,1,Length(Indent)-2);
                    end;
            ibsymref : begin
                         readsymref;
                         readref;
                       end;
            ibdefref : begin
                         readdefref;
                         readref;
                         if (ppufile^.header.flags and uf_local_browser)<>0 then
                           begin
                             { parast and localst }
                             indent:=indent+'  ';
                             Writeln(indent,'Parasymtable for function');
                             readdefinitions(false);
                             readsymbols;
                             b:=ppufile^.readentry;
                             if b=ibbeginsymtablebrowser then
                               readbrowser;
                             Writeln(indent,'Localsymtable for function');
                             readdefinitions(false);
                             readsymbols;
                             b:=ppufile^.readentry;
                             if b=ibbeginsymtablebrowser then
                               readbrowser;
                             Indent:=Copy(Indent,1,Length(Indent)-2);
                           end;
                       end;
             iberror : begin
                         Writeln('Error in PPU');
                         exit;
                       end;
        ibendsymtablebrowser : break;
       else
        begin
        WriteLn('!! Skipping unsupported PPU Entry in Browser: ',b);
        Halt;
        end;
       end;
     until false;
   end;
  Indent:=Copy(Indent,1,Length(Indent)-2);
  Writeln(Indent,'End of symtable browser');
end;




procedure dofile (filename : string);
var
  b,unitindex : byte;
begin
{ reset }
  space:='';
  defcnt:=0;
  symcnt:=0;
{ fix filename }
  if pos('.',filename)=0 then
   filename:=filename+'.ppu';
  ppufile:=new(pppufile,Init(filename));
  if not ppufile^.open then
   begin
     writeln ('IO-Error when opening : ',filename,', Skipping');
     exit;
   end;
{ PPU File is open, check for PPU Id }
  if not ppufile^.CheckPPUID then
   begin
     writeln(Filename,' : Not a valid PPU file, Skipping');
     exit;
   end;
{ Check PPU Version }
  Writeln('Analyzing ',filename,' (v',ppufile^.GetPPUVersion,')');
  if ppufile^.GetPPUVersion<16 then
   begin
     writeln(Filename,' : Old PPU Formats (<v16) are not supported, Skipping');
     exit;
   end;
{ Write PPU Header Information }
  if (verbose and v_header)<>0 then
   begin
     Writeln;
     Writeln('Header');
     Writeln('-------');
     with ppufile^.header do
      begin
        Writeln('Compiler version        : ',hi(ppufile^.header.compiler and $ff),'.',lo(ppufile^.header.compiler));
        WriteLn('Target processor        : ',Cpu2Str(cpu));
        WriteLn('Target operating system : ',Target2Str(target));
        Write  ('Unit flags              : ',flags,', ');
          if (flags and uf_init)<>0 then
           write('init ');
          if (flags and uf_big_endian)<>0 then
           write('big_endian ');
          if (flags and uf_finalize)<>0 then
           write('finalize ');
          if (flags and uf_has_dbx)<>0 then
           write('has_dbx ');
          if (flags and uf_has_browser)<>0 then
           write('has_browser ');
          if (flags and uf_smartlink)<>0 then
           write('smartlink ');
          if (flags and uf_in_library)<>0 then
           write('in_library ');
          if (flags and uf_shared_linked)<>0 then
           write('shared_linked ');
          if (flags and uf_static_linked)<>0 then
           write('static_linked ');
          if (flags and uf_local_browser)<>0 then
           write('local_browser ');
          if (flags and uf_obj_linked)<>0 then
           write('obj_linked ');
          if (flags=0) then
           write('(none)');
          writeln;
        Writeln('FileSize (w/o header)   : ',size);
        Writeln('Checksum                : ',checksum);
        Writeln('Interface Checksum      : ',interface_checksum);
      end;
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
   ppufile^.skipuntilentry(ibendinterface);
{read the definitions}
  if (verbose and v_defs)<>0 then
   begin
     Writeln;
     Writeln('Interface definitions');
     Writeln('----------------------');
     readdefinitions(false);
   end
  else
   ppufile^.skipuntilentry(ibenddefs);
{read the symbols}
  if (verbose and v_syms)<>0 then
   begin
     Writeln;
     Writeln('Interface Symbols');
     Writeln('------------------');
     readsymbols;
   end
  else
   ppufile^.skipuntilentry(ibendsyms);
{read the implementation stuff}
{ Not used at the moment (PFV)
  if (verbose and v_implementation)<>0 then
   begin
     Writeln;
     Writeln('Implementation section');
     Writeln('-----------------------');
     readimplementation;
   end
  else}
   ppufile^.skipuntilentry(ibendimplementation);
{read the static browser units stuff}
  if (ppufile^.header.flags and uf_local_browser)<>0 then
   begin
     if (verbose and v_defs)<>0 then
      begin
        Writeln;
        Writeln('Static definitions');
        Writeln('----------------------');
        readdefinitions(false);
      end
     else
      ppufile^.skipuntilentry(ibenddefs);
   {read the symbols}
     if (verbose and v_syms)<>0 then
      begin
        Writeln;
        Writeln('Static Symbols');
        Writeln('------------------');
        readsymbols;
      end;
   end;
{read the browser units stuff}
  if (ppufile^.header.flags and uf_has_browser)<>0 then
   begin
     if (verbose and v_browser)<>0 then
      begin
        Writeln;
        Writeln('Browser section');
        Writeln('---------------');
        UnitIndex:=0;
        repeat
          b:=ppufile^.readentry;
          if b = ibendbrowser then break;
          if b=ibbeginsymtablebrowser then
            begin
               Writeln('Unit ',UnitIndex);
               readbrowser;
               Inc(UnitIndex);
            end
          else
            Writeln('Wrong end browser entry ',b,' should be ',ibendbrowser);
        until false;
      end;
   end;
{read the static browser units stuff}
  if (ppufile^.header.flags and uf_local_browser)<>0 then
   begin
     if (verbose and v_browser)<>0 then
      begin
        Writeln;
        Writeln('Static browser section');
        Writeln('---------------');
        b:=ppufile^.readentry;
        if b=ibbeginsymtablebrowser then
          begin
             Writeln('Unit ',UnitIndex);
             readbrowser;
             Inc(UnitIndex);
          end
        else
          Writeln('Wrong end browser entry ',b,' should be ',ibendbrowser);
      end;
   end;
{shutdown ppufile}
  ppufile^.close;
  dispose(ppufile,done);
  Writeln;
end;



procedure help;
begin
  writeln('usage: ppudump [options] <filename1> <filename2>...');
  writeln;
  writeln('[options] can be:');
  writeln('    -V<verbose>  Set verbosity to <verbose>');
  writeln('                   H - Show header info');
  writeln('                   I - Show interface');
  writeln('                   M - Show implementation');
  writeln('                   S - Show interface symbols');
  writeln('                   D - Show interface definitions');
  writeln('                   B - Show browser info');
  writeln('                   A - Show all');
  writeln('    -?           This helpscreen');
  halt;
end;

var
  startpara,
  nrfile,i  : longint;
  para      : string;
begin
  writeln(Title+' '+Version);
  writeln(Copyright);
  writeln;
  if paramcount<1 then
   begin
     writeln('usage: dumpppu [options] <filename1> <filename2>...');
     halt(1);
   end;
{ turn verbose on by default }
  verbose:=v_all;
{ read options }
  startpara:=1;
  while copy(paramstr(startpara),1,1)='-' do
   begin
     para:=paramstr(startpara);
     case upcase(para[2]) of
      'V' : begin
              verbose:=0;
              for i:=3to length(para) do
               case upcase(para[i]) of
                'H' : verbose:=verbose or v_header;
                'I' : verbose:=verbose or v_interface;
                'M' : verbose:=verbose or v_implementation;
                'D' : verbose:=verbose or v_defs;
                'S' : verbose:=verbose or v_syms;
                'B' : verbose:=verbose or v_browser;
                'A' : verbose:=verbose or v_all;
               end;
            end;
      '?' : help;
     end;
     inc(startpara);
   end;
{ process files }
  for nrfile:=startpara to paramcount do
   dofile (paramstr(nrfile));
  if has_errors then
    Halt(1);
end.
{
  $Log$
  Revision 1.3  1999-06-08 22:16:06  peter
    * version 0.99.12

  Revision 1.2  1999/05/14 17:52:04  peter
    * new deref

  Revision 1.1  1999/05/12 16:11:39  peter
    * moved

  Revision 1.31  1999/04/29 17:22:34  peter
    * fixed property sym

  Revision 1.30  1999/04/26 18:27:39  peter
    * more updates

  Revision 1.29  1999/04/26 13:30:44  peter
    * support new unit format

  Revision 1.28  1999/04/26 09:35:04  peter
    * support for v16

  Revision 1.27  1999/03/16 21:00:03  peter
    * fixed varsym

  Revision 1.26  1999/03/02 22:54:54  peter
    * merged some of my code back which was changed after pierres commit

  Revision 1.25  1999/03/02 14:21:44  pierre
   * adapted to symtable changes mainly local browser

  Revision 1.24  1999/03/02 13:48:16  peter
    * better procoptions display and easy maintainance
    * removed implementation section printing, becuase it's not used
    * linkunitfiles support

  Revision 1.23  1999/02/22 13:22:00  pierre
   + static browser reading

  Revision 1.22  1999/02/16 00:48:42  peter
    * updated for new flags

  Revision 1.21  1998/11/28 16:21:02  peter
    + support for dll variables

  Revision 1.20  1998/11/12 11:36:43  peter
    * fixed target name

  Revision 1.19  1998/11/04 10:17:42  peter
    + const,var,value is written for parameters

  Revision 1.18  1998/10/13 13:06:14  peter
    * updated for new target enum

  Revision 1.17  1998/09/25 09:51:53  peter
    + symtable size and # of symbols

  Revision 1.16  1998/09/24 23:24:11  peter
    * small update to support ibstartdef

  Revision 1.12  1998/09/21 08:45:28  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.11  1998/09/18 08:01:42  pierre
    + improvement on the usebrowser part
      (does not work correctly for now)

  Revision 1.10  1998/09/01 17:35:33  peter
    * update for new po's

  Revision 1.9  1998/09/01 12:46:52  peter
    + enum savesize

  Revision 1.8  1998/08/26 10:01:18  peter
    + set support

  Revision 1.7  1998/08/20 13:01:41  peter
    + object_options, new enumdef

  Revision 1.6  1998/08/17 10:26:28  peter
    * updated for new shared/static style

  Revision 1.5  1998/08/13 10:56:28  peter
    * check if a whole entry is read
    + support for constset

  Revision 1.4  1998/08/11 15:31:44  peter
    * write extended to ppu file
    * new version 0.99.7

  Revision 1.3  1998/07/10 10:59:17  peter
    + m68k target support

  Revision 1.2  1998/06/17 13:58:28  peter
    + symbol/def nrs are now listed

  Revision 1.1  1998/06/13 00:05:01  peter
    + new util to dump v15+ ppu

}
