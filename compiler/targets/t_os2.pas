{
    $Id$
    Copyright (c) 1998-2000 by Daniel Mantione
    Portions Copyright (c) 1998-2000 Eberhard Mattes

    Unit to write out import libraries and def files for OS/2

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

 ****************************************************************************
}
{
   A lot of code in this unit has been ported from C to Pascal from the
   emximp utility, part of the EMX development system. Emximp is copyrighted
   by Eberhard Mattes. Note: Eberhard doesn't know much about the Pascal
   port, please send questions to Daniel Mantione
   <d.s.p.mantione@twi.tudelft.nl>.
}
unit t_os2;

{$i defines.inc}

interface


implementation

  uses
{$ifdef Delphi}
     sysutils,
     dmisc,
{$else Delphi}
     strings,
     dos,
{$endif Delphi}
     cutils,cclasses,
     globtype,comphook,systems,symsym,
     globals,verbose,fmodule,script,
     import,link;

  type
    timportlibos2=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(const func,module:string;index:longint;const name:string);override;
      procedure generatelib;override;
    end;

    tlinkeros2=class(tlinker)
    private
       Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
       constructor Create;override;
       procedure SetDefaultInfo;override;
       function  MakeExecutable:boolean;override;
    end;


const   profile_flag:boolean=false;

const   n_ext   = 1;
        n_abs   = 2;
        n_text  = 4;
        n_data  = 6;
        n_bss   = 8;
        n_imp1  = $68;
        n_imp2  = $6a;

type    reloc=packed record     {This is the layout of a relocation table
                                 entry.}
            address:longint;    {Fixup location}
            remaining:longint;
            {Meaning of bits for remaining:
             0..23:              Symbol number or segment
             24:                 Self-relative fixup if non-zero
             25..26:             Fixup size (0: 1 byte, 1: 2, 2: 4 bytes)
             27:                 Reference to symbol or segment
             28..31              Not used}
        end;

        nlist=packed record     {This is the layout of a symbol table entry.}
            strofs:longint;     {Offset in string table}
            typ:byte;           {Type of the symbol}
            other:byte;         {Other information}
            desc:word;          {More information}
            value:longint;      {Value (address)}
        end;

        a_out_header=packed record
            magic:word;         {Magic word, must be $0107}
            machtype:byte;      {Machine type}
            flags:byte;         {Flags}
            text_size:longint;  {Length of text, in bytes}
            data_size:longint;  {Length of initialized data, in bytes}
            bss_size:longint;   {Length of uninitialized data, in bytes}
            sym_size:longint;   {Length of symbol table, in bytes}
            entry:longint;      {Start address (entry point)}
            trsize:longint;     {Length of relocation info for text, bytes}
            drsize:longint;     {Length of relocation info for data, bytes}
        end;

        ar_hdr=packed record
            ar_name:array[0..15] of char;
            ar_date:array[0..11] of char;
            ar_uid:array[0..5] of char;
            ar_gid:array[0..5] of char;
            ar_mode:array[0..7] of char;
            ar_size:array[0..9] of char;
            ar_fmag:array[0..1] of char;
        end;

var aout_str_size:longint;
    aout_str_tab:array[0..2047] of byte;
    aout_sym_count:longint;
    aout_sym_tab:array[0..5] of nlist;

    aout_text:array[0..63] of byte;
    aout_text_size:longint;

    aout_treloc_tab:array[0..1] of reloc;
    aout_treloc_count:longint;

    aout_size:longint;
    seq_no:longint;

    ar_member_size:longint;

    out_file:file;

procedure write_ar(const name:string;size:longint);

var ar:ar_hdr;
    time:datetime;
    dummy:word;
    numtime:longint;
    tmp:string[19];


begin
    ar_member_size:=size;
    fillchar(ar.ar_name,sizeof(ar.ar_name),' ');
    move(name[1],ar.ar_name,length(name));
    getdate(time.year,time.month,time.day,dummy);
    gettime(time.hour,time.min,time.sec,dummy);
    packtime(time,numtime);
    str(numtime,tmp);
    fillchar(ar.ar_date,sizeof(ar.ar_date),' ');
    move(tmp[1],ar.ar_date,length(tmp));
    ar.ar_uid:='0     ';
    ar.ar_gid:='0     ';
    ar.ar_mode:='100666'#0#0;
    str(size,tmp);
    fillchar(ar.ar_size,sizeof(ar.ar_size),' ');
    move(tmp[1],ar.ar_size,length(tmp));
    ar.ar_fmag:='`'#10;
    blockwrite(out_file,ar,sizeof(ar));
end;

procedure finish_ar;

var a:byte;

begin
    a:=0;
    if odd(ar_member_size) then
        blockwrite(out_file,a,1);
end;

procedure aout_init;

begin
  aout_str_size:=sizeof(longint);
  aout_sym_count:=0;
  aout_text_size:=0;
  aout_treloc_count:=0;
end;

function aout_sym(const name:string;typ,other:byte;desc:word;
                  value:longint):longint;

begin
    if aout_str_size+length(name)+1>sizeof(aout_str_tab) then
        Do_halt($da);
    if aout_sym_count>=sizeof(aout_sym_tab) div sizeof(aout_sym_tab[0]) then
        Do_halt($da);
    aout_sym_tab[aout_sym_count].strofs:=aout_str_size;
    aout_sym_tab[aout_sym_count].typ:=typ;
    aout_sym_tab[aout_sym_count].other:=other;
    aout_sym_tab[aout_sym_count].desc:=desc;
    aout_sym_tab[aout_sym_count].value:=value;
    strPcopy(@aout_str_tab[aout_str_size],name);
    aout_str_size:=aout_str_size+length(name)+1;
    aout_sym:=aout_sym_count;
    inc(aout_sym_count);
end;

procedure aout_text_byte(b:byte);

begin
    if aout_text_size>=sizeof(aout_text) then
        Do_halt($da);
    aout_text[aout_text_size]:=b;
    inc(aout_text_size);
end;

procedure aout_text_dword(d:longint);

type li_ar=array[0..3] of byte;

begin
    aout_text_byte(li_ar(d)[0]);
    aout_text_byte(li_ar(d)[1]);
    aout_text_byte(li_ar(d)[2]);
    aout_text_byte(li_ar(d)[3]);
end;

procedure aout_treloc(address,symbolnum,pcrel,len,ext:longint);

begin
    if aout_treloc_count>=sizeof(aout_treloc_tab) div sizeof(reloc) then
        Do_halt($da);
    aout_treloc_tab[aout_treloc_count].address:=address;
    aout_treloc_tab[aout_treloc_count].remaining:=symbolnum+pcrel shl 24+
     len shl 25+ext shl 27;
    inc(aout_treloc_count);
end;

procedure aout_finish;

begin
    while (aout_text_size and 3)<>0 do
        aout_text_byte ($90);
    aout_size:=sizeof(a_out_header)+aout_text_size+aout_treloc_count*
     sizeof(reloc)+aout_sym_count*sizeof(aout_sym_tab[0])+aout_str_size;
end;

procedure aout_write;

var ao:a_out_header;

begin
    ao.magic:=$0107;
    ao.machtype:=0;
    ao.flags:=0;
    ao.text_size:=aout_text_size;
    ao.data_size:=0;
    ao.bss_size:=0;
    ao.sym_size:=aout_sym_count*sizeof(aout_sym_tab[0]);
    ao.entry:=0;
    ao.trsize:=aout_treloc_count*sizeof(reloc);
    ao.drsize:=0;
    blockwrite(out_file,ao,sizeof(ao));
    blockwrite(out_file,aout_text,aout_text_size);
    blockwrite(out_file,aout_treloc_tab,sizeof(reloc)*aout_treloc_count);
    blockwrite(out_file,aout_sym_tab,sizeof(aout_sym_tab[0])*aout_sym_count);
    longint((@aout_str_tab)^):=aout_str_size;
    blockwrite(out_file,aout_str_tab,aout_str_size);
end;

procedure timportlibos2.preparelib(const s:string);

{This code triggers a lot of bugs in the compiler.
const   armag='!<arch>'#10;
        ar_magic:array[1..length(armag)] of char=armag;}
const   ar_magic:array[1..8] of char='!<arch>'#10;
var
  libname : string;
begin
    libname:=FixFileName(s+'.ao2');
    seq_no:=1;
    current_module.linkunitstaticlibs.add(libname,link_allways);
    assign(out_file,current_module.outputpath^+libname);
    rewrite(out_file,1);
    blockwrite(out_file,ar_magic,sizeof(ar_magic));
end;

procedure timportlibos2.importprocedure(const func,module:string;index:longint;const name:string);
{func       = Name of function to import.
 module     = Name of DLL to import from.
 index      = Index of function in DLL. Use 0 to import by name.
 name       = Name of function in DLL. Ignored when index=0;}
var tmp1,tmp2,tmp3:string;
    sym_mcount,sym_import:longint;
    fixup_mcount,fixup_import:longint;
begin
    { force the current mangledname }
    aktprocdef.has_mangledname:=true;

    aout_init;
    tmp2:=func;
    if profile_flag and not (copy(func,1,4)='_16_') then
        begin
            {sym_entry:=aout_sym(func,n_text+n_ext,0,0,aout_text_size);}
            sym_mcount:=aout_sym('__mcount',n_ext,0,0,0);
            {Use, say, "_$U_DosRead" for "DosRead" to import the
             non-profiled function.}
            tmp2:='__$U_'+func;
            sym_import:=aout_sym(tmp2,n_ext,0,0,0);
            aout_text_byte($55);    {push ebp}
            aout_text_byte($89);    {mov ebp, esp}
            aout_text_byte($e5);
            aout_text_byte($e8);    {call _mcount}
            fixup_mcount:=aout_text_size;
            aout_text_dword(0-(aout_text_size+4));
            aout_text_byte($5d);    {pop ebp}
            aout_text_byte($e9);    {jmp _$U_DosRead}
            fixup_import:=aout_text_size;
            aout_text_dword(0-(aout_text_size+4));

            aout_treloc(fixup_mcount,sym_mcount,1,2,1);
            aout_treloc (fixup_import, sym_import,1,2,1);
        end;
    str(seq_no,tmp1);
    tmp1:='IMPORT#'+tmp1;
    if name='' then
        begin
            str(index,tmp3);
            tmp3:=func+'='+module+'.'+tmp3;
        end
    else
        tmp3:=func+'='+module+'.'+name;
    aout_sym(tmp2,n_imp1+n_ext,0,0,0);
    aout_sym(tmp3,n_imp2+n_ext,0,0,0);
    aout_finish;
    write_ar(tmp1,aout_size);
    aout_write;
    finish_ar;
    inc(seq_no);
end;

procedure timportlibos2.generatelib;

begin
    close(out_file);
end;


{****************************************************************************
                               TLinkeros2
****************************************************************************}

Constructor TLinkeros2.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;


procedure TLinkeros2.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ld $OPT -o $EXE.out @$RES';
     ExeCmd[2]:='emxbind -b $STRIP $APPTYPE $RSRC -k$STACKKB -h$HEAPMB -o $EXE.exe $EXE.out -aim -s$DOSHEAPKB';
     ExeCmd[3]:='del $EXE.out';
   end;
end;


Function TLinkeros2.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TStringListItem;
  s        : string;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+HPath.Str);
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('-L'+HPath.Str);
     HPath:=TStringListItem(HPath.Next);
   end;

  { add objectfiles, start with prt0 always }
  LinkRes.AddFileName(FindObjectFile('prt0',''));
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.AddFileName(s);
   end;

  { Write staticlibraries }
  { No group !! This will not work correctly PM }
  While not StaticLibFiles.Empty do
   begin
     S:=StaticLibFiles.GetFirst;
     LinkRes.AddFileName(s)
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  While not SharedLibFiles.Empty do
   begin
     S:=SharedLibFiles.GetFirst;
     i:=Pos(target_info.sharedlibext,S);
     if i>0 then
      Delete(S,i,255);
     LinkRes.Add('-l'+s);
   end;

{ Write and Close response }
  linkres.writetodisk;
  LinkRes.Free;

  WriteResponseFile:=True;
end;


function TLinkeros2.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  success : boolean;
  i       : longint;
  AppTypeStr,
  StripStr: string[40];
  RsrcStr : string;
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  if (cs_link_strip in aktglobalswitches) then
   StripStr := '-s'
  else
   StripStr := '';
  if (usewindowapi) or (AppType = app_gui) then
   AppTypeStr := '-p'
  else if AppType = app_fs then
   AppTypeStr := '-f'
  else AppTypeStr := '-w';
  if not (Current_module.ResourceFiles.Empty) then
   RsrcStr := '-r ' + Current_module.ResourceFiles.GetFirst
  else
   RsrcStr := '';
(* Only one resource file supported, discard everything else
   (should be already empty anyway, however. *)
  Current_module.ResourceFiles.Clear;
{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  success:=false;
  for i:=1 to 3 do
   begin
     SplitBinCmd(Info.ExeCmd[i],binstr,cmdstr);
     if binstr<>'' then
      begin
        { Is this really required? Not anymore according to my EMX docs }
        Replace(cmdstr,'$HEAPMB',tostr((heapsize+1048575) shr 20));
        {Size of the stack when an EMX program runs in OS/2.}
        Replace(cmdstr,'$STACKKB',tostr((stacksize+1023) shr 10));
        {When an EMX program runs in DOS, the heap and stack share the
         same memory pool. The heap grows upwards, the stack grows downwards.}
        Replace(cmdstr,'$DOSHEAPKB',tostr((stacksize+heapsize+1023) shr 10));
        Replace(cmdstr,'$STRIP',StripStr);
        Replace(cmdstr,'$APPTYPE',AppTypeStr);
        Replace(cmdstr,'$RES',outputexedir+Info.ResName);
        Replace(cmdstr,'$OPT',Info.ExtraOptions);
        Replace(cmdstr,'$RSRC',RsrcStr);
        Replace(cmdstr,'$EXE',current_module.exefilename^);
        if i<>3 then
         success:=DoExec(FindUtil(binstr),cmdstr,(i=1),false)
        else
         success:=DoExec(binstr,cmdstr,(i=1),true);
(* We still want to have the PPAS script complete, right?
        if not success then
         break;
*)
      end;
   end;

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   RemoveFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

    const
       res_emxbind_info : tresinfo =
          (
            id     : res_emxbind;
            resbin : 'emxbind';
            rescmd : '-b -r $RES $OBJ'
(* Not really used - see TLinkeros2.SetDefaultInfo in t_os2.pas. *)
          );

    const
       target_i386_os2_info : ttargetinfo =
          (
            target       : target_i386_OS2;
            name         : 'OS/2 via EMX';
            shortname    : 'OS2';
            flags        : [tf_need_export];
            cpu          : cpu_i386;
            unit_env     : 'OS2UNITS';
            extradefines : '';
            sourceext    : '.pas';
            pasext       : '.pp';
            exeext       : '.exe';
            defext       : '.def';
            scriptext    : '.cmd';
            smartext     : '.sl';
            unitext      : '.ppo';
            unitlibext   : '.ppl';
            asmext       : '.so2';
            objext       : '.oo2';
            resext       : '.res';
            resobjext    : '.oor';
            sharedlibext : '.ao2';
            staticlibext : '.ao2';
            staticlibprefix : '';
            sharedlibprefix : '';
            sharedClibext : 'dll';
            staticClibext : '.a';
            staticClibprefix : '';
            sharedClibprefix : '';
            Cprefix      : '_';
            newline      : #13#10;
            dirsep       : '\';
            files_case_relevent : false;
            assem        : as_i386_as_aout;
            assemextern  : as_i386_as_aout;
            link         : ld_i386_os2;
            linkextern   : ld_i386_os2;
            ar           : ar_gnu_ar;
            res          : res_emxbind;
            script       : script_dos;
            endian       : endian_little;
            alignment    :
              (
                procalign       : 4;
                loopalign       : 4;
                jumpalign       : 0;
                constalignmin   : 0;
                constalignmax   : 4;
                varalignmin     : 0;
                varalignmax     : 4;
                localalignmin   : 0;
                localalignmax   : 4;
                paraalign       : 4;
                recordalignmin  : 0;
                recordalignmax  : 2;
                maxCrecordalign : 4
              );
            first_parm_offset : 8;
            heapsize     : 256*1024;
            stacksize    : 256*1024;
            DllScanSupported:true;
            use_bound_instruction : false;
            use_function_relative_addresses : false
          );


initialization
  RegisterLinker(ld_i386_os2,TLinkerOS2);
  RegisterImport(target_i386_os2,TImportLibOS2);
  RegisterRes(res_emxbind_info);
  RegisterTarget(target_i386_os2_info);
end.
{
  $Log$
  Revision 1.17  2002-04-20 21:43:18  carl
  * fix stack size for some targets
  + add offset to parameters from frame pointer info.
  - remove some unused stuff

  Revision 1.16  2002/04/15 19:16:57  carl
  - remove size_of_pointer field

  Revision 1.15  2002/04/04 19:06:13  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.14  2002/01/29 21:27:34  peter
    * default alignment changed to 4 bytes for locals and static const,var

  Revision 1.12  2002/01/27 12:58:42  hajny
    * fix for a problem with importprocedure

  Revision 1.11  2001/09/18 11:32:00  michael
  * Fixes win32 linking problems with import libraries
  * LINKLIB Libraries are now looked for using C file extensions
  * get_exepath fix

  Revision 1.10  2001/09/17 21:29:16  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.9  2001/08/07 18:47:15  peter
    * merged netbsd start
    * profile for win32

  Revision 1.8  2001/07/01 20:16:20  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.7  2001/06/28 19:46:25  peter
    * added override and virtual for constructors

  Revision 1.6  2001/06/03 15:15:32  peter
    * dllprt0 stub for linux shared libs
    * pass -init and -fini for linux shared libs
    * libprefix splitted into staticlibprefix and sharedlibprefix

  Revision 1.5  2001/06/02 19:22:44  peter
    * extradefines field added

  Revision 1.4  2001/04/18 22:02:04  peter
    * registration of targets and assemblers

  Revision 1.3  2001/04/13 01:22:22  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.2  2001/02/27 19:40:05  hajny
    * a.out deleted upon successful binding

  Revision 1.1  2001/02/26 19:43:11  peter
    * moved target units to subdir

  Revision 1.7  2001/01/20 18:32:52  hajny
    + APPTYPE support under OS/2, app_fs, GetEnvPChar for OS/2

  Revision 1.6  2000/12/25 00:07:30  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.5  2000/09/24 15:06:31  peter
    * use defines.inc

  Revision 1.4  2000/09/20 19:38:34  peter
    * fixed staticlib filename and unitlink instead of otherlinky

  Revision 1.3  2000/08/27 16:11:54  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:50  michael
  + removed logs

}
