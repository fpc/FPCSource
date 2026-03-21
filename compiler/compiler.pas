{
    This unit is the interface of the compiler which can be used by
    external programs to link in the compiler

    Copyright (c) 1998-2005 by Florian Klaempfl

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

unit compiler;

{$i fpcdefs.inc}

{ some units are implicitly needed by the compiler }
{$WARN 5023 off : Unit "$1" not used in $2}

interface

uses
  compilerbase,
{$ifdef GO32V2}
  emu387,
{$endif GO32V2}
{$ifdef WATCOM}
  emu387,
{$endif WATCOM}
{$if defined(unix)}
  { system code page stuff for unix }
  unixcp,
  fpwidestring,
{$endif}
{$IFNDEF USE_FAKE_SYSUTILS}
  sysutils,math,
{$ELSE}
  fksysutl,
{$ENDIF}
  verbose,comphook,systems,
  cutils,cfileutl,cclasses,globals,options,switches,fmodule,parser,symtable,
  assemble,link,dbgbase,import,export,tokens,wpo
  { cpu parameter handling }
  ,cpupara
  { procinfo stuff }
  ,cpupi
  { cpu codegenerator }
  ,cgcpu
{$ifndef NOPASS2}
  ,cpunode
{$endif}
  { cpu targets }
  ,cputarg
{$ifdef llvm}
  ,llvmtarg
{$endif llvm}
  { system information for source system }
  { the information about the target os  }
  { are pulled in by the t_* units       }
{$ifndef GENERIC_CPU}
{$ifdef amiga}
  ,i_amiga
{$endif amiga}
{$ifdef android}
  ,i_android
{$endif android}
{$ifdef aros}
  ,i_aros
{$endif}
{$ifdef atari}
  ,i_atari
{$endif atari}
{$ifdef beos}
  ,i_beos
{$endif beos}
{$ifdef bsd}
{$ifdef darwin}
  ,i_darwin
{$else darwin}
  ,i_bsd
{$endif darwin}
{$endif bsd}
{$ifdef gba}
  ,i_gba
{$endif gba}
{$ifdef go32v2}
  ,i_go32v2
{$endif go32v2}
{$ifdef haiku}
  ,i_haiku
{$endif haiku}
{$ifdef human68k}
  ,i_human68k
{$endif human68k}
{$ifdef linux}
  ,i_linux
{$endif linux}
{$ifdef macos}
  ,i_macos
{$endif macos}
{$ifdef morphos}
  ,i_morph
{$endif morphos}
{$ifdef nds}
  ,i_nds
{$endif nds}
{$ifdef nwm}
  ,i_nwm
{$endif nwm}
{$ifdef nwl}
  ,i_nwl
{$endif nwm}
{$ifdef os2}
 {$ifdef emx}
  ,i_emx
 {$else emx}
  ,i_os2
 {$endif emx}
{$endif os2}
{$ifdef palmos}
  ,i_palmos
{$endif palmos}
{$ifdef solaris}
  ,i_sunos
{$endif solaris}
{$ifdef sinclairql}
  ,i_sinclairql
{$endif sinclairql}
{$ifdef wdosx}
  ,i_wdosx
{$endif wdosx}
{$ifdef wii}
  ,i_wii
{$endif wii}
{$ifdef windows}
  ,i_win
{$endif windows}
{$ifdef symbian}
  ,i_symbian
{$endif symbian}
{$ifdef nativent}
  ,i_nativent
{$endif nativent}
{$ifdef aix}
  ,i_aix
{$endif aix}
{$endif GENERIC_CPU}
  ,ctask
  ,globtype,compinnr,cpuinfo,constexp,widestr,blockutl,pkgutil,procdefutil
  ,hlcgobj,cgobj
  ,ngenutil,pgentype,objcgutl,objcutil,ncgrtti,cgexcept,paramgr
  ,opt,optloop
  ,aasmdata
  ,symbase,symtype,symsym,symdef,symconst
  ,node,nadd,nbas,ncal,ncnv,ncon,nflw,ninl,nld,nmat,nmem,nobjc,nopt,nset;

type
{****************************************************************************
                                TCompiler
****************************************************************************}

  { TCompiler }

  TCompiler = class(TCompilerBase)
  private
    FTarget: TCompilerTarget;
    FTime: TCompilerTime;
    FGlobals: TCompilerGlobals;
    FVerbose: TVerbose;
    FTaskHandler: TTask_handler;
    FParser: TParser;
    FNodeUtils: TNodeUtils;
    FOpt: TOptimizers;
    FObjCGUtl: TObjCCodeGenUtils;
    FBlockUtl: TBlockUtils;
    FPkgUtil: TPackageUtils;
    FProcDefUtil: TProcDefUtils;
    FObjCUtil: TObjectiveCUtils;
    FOptions: TOptions;
    FRTTIWriter : TRTTIWriter;
    FLinker: TLinker;
    FExceptionStateHandler: tcgexceptionstatehandler;
    Fhlcg: thlcgobj;
    Fcg: tcg;
{$ifdef cpu64bitalu}
    { Code generator class for all operations working with 128-Bit operands }
    Fcg128 : tcg128;
{$else cpu64bitalu}
    { Code generator class for all operations working with 64-Bit operands }
    Fcg64 : tcg64;
{$endif cpu64bitalu}
    Fparamanager : tparamanager;

    Finitialmacrosymtable: TSymtable;   { macros initially defined by the compiler or
                                          given on the command line. Is common
                                          for all files compiled and do not change. }
    Fmacrosymtablestack,
    Fsymtablestack        : TSymtablestack;

    FSysSymList: tsyssymlist;

    { Current assignment node }
    Faktassignmentnode : tassignmentnode;

    CompilerInitedAfterArgs,
    CompilerInited : boolean;

    procedure InitCompiler(const cmd:TCmdStr);
    procedure DoneCompiler;
  public
    function Compile(const cmd:TCmdStr):longint;

    procedure CreateExceptionStateHandler(eshclass: tcgexceptionstatehandlerclass);

    procedure InitLinker;
    procedure DoneLinker;

    property Target: TCompilerTarget read FTarget;
    property Time: TCompilerTime read FTime;
    property Verbose: TVerbose read FVerbose;
    property Globals: TCompilerGlobals read FGlobals;
    property Parser: TParser read FParser;
    property NodeUtils: TNodeUtils read FNodeUtils;
    property Opt: TOptimizers read FOpt;
    property ObjCGUtl: TObjCCodeGenUtils read FObjCGUtl;
    property BlockUtl: TBlockUtils read FBlockUtl;
    property PkgUtil: TPackageUtils read FPkgUtil;
    property ProcDefUtil: TProcDefUtils read FProcDefUtil;
    property ObjCUtil: TObjectiveCUtils read FObjCUtil;
    property Options: TOptions read FOptions;
    property RTTIWriter : TRTTIWriter read FRTTIWriter write FRTTIWriter;
    property Linker: TLinker read FLinker;
    property ExceptionStateHandler: tcgexceptionstatehandler read FExceptionStateHandler;
    {# Main high level code generator class }
    property hlcg: thlcgobj read Fhlcg write Fhlcg;
    { Main code generator class }
    property cg: tcg read Fcg write Fcg;
{$ifdef cpu64bitalu}
    { Code generator class for all operations working with 128-Bit operands }
    property cg128 : tcg128 read Fcg128 write Fcg128;
{$else cpu64bitalu}
    { Code generator class for all operations working with 64-Bit operands }
    property cg64 : tcg64 read Fcg64 write Fcg64;
{$endif cpu64bitalu}
    property paramanager: tparamanager read Fparamanager;
    property initialmacrosymtable: TSymtable read Finitialmacrosymtable write Finitialmacrosymtable;
    property macrosymtablestack: TSymtablestack read Fmacrosymtablestack write Fmacrosymtablestack;
    property symtablestack: TSymtablestack read Fsymtablestack write Fsymtablestack;
    property syssymlist: tsyssymlist read FSysSymList;
    property aktassignmentnode : tassignmentnode read Faktassignmentnode write Faktassignmentnode;
  end;

  { TCompilerHelper }

  TCompilerHelper = class helper for TCompilerBase
  private
    function Getaktassignmentnode: tassignmentnode; inline;
    function GetBlockUtl: TBlockUtils; inline;
    function GetCG: tcg; inline;
{$ifdef cpu64bitalu}
    function GetCG128 : tcg128; inline;
{$else cpu64bitalu}
    function GetCG64 : tcg64; inline;
{$endif cpu64bitalu}
    function GetExceptionStateHandler: tcgexceptionstatehandler; inline;
    function GetGlobals: TCompilerGlobals; inline;
    function GetHLCG: thlcgobj; inline;
    function Getinitialmacrosymtable: TSymtable; inline;
    function GetLinker: TLinker; inline;
    function Getmacrosymtablestack: TSymtablestack; inline;
    function GetObjCGUtl: TObjCCodeGenUtils; inline;
    function GetObjCUtil: TObjectiveCUtils; inline;
    function GetParaManager: tparamanager; inline;
    function GetParser: TParser; inline;
    function GetNodeUtils: TNodeUtils; inline;
    function GetOpt: TOptimizers; inline;
    function GetPkgUtil: TPackageUtils; inline;
    function GetProcDefUtil: TProcDefUtils; inline;
    function GetRTTIWriter: TRTTIWriter; inline;
    function Getsymtablestack: TSymtablestack; inline;
    function GetSysSymList: tsyssymlist; inline;
    function GetTarget: TCompilerTarget; inline;
    function GetTime: TCompilerTime; inline;
    function GetVerbose: TVerbose; inline;
    procedure Setaktassignmentnode(AValue: tassignmentnode); inline;
  public
    { node constructor helpers }
    { nadd }
    function caddnode(tt : tnodetype;l,r : tnode):taddnode; inline;
    function caddnode_internal(tt:tnodetype;l,r:tnode):taddnode; inline;
    { nbas }
    function cnothingnode:tnothingnode; inline;
    function cerrornode:terrornode; inline;
    function cspecializenode(l:tnode;g:boolean;s:tsym;u:boolean):tspecializenode; inline;
    function cspecializenode_inherited(l:tnode;g:boolean;s:tsym;i:tdef):tspecializenode; inline;
    function cfinalizetempsnode:tfinalizetempsnode; inline;
    function casmnode(p : TAsmList):tasmnode; inline;
    function casmnode_get_position:tasmnode; inline;
    function cstatementnode(l,r : tnode):tstatementnode; inline;
    function cblocknode(l : tnode):tblocknode; inline;
    function ctempcreatenode(_typedef: tdef; _size: tcgint; _temptype: ttemptype;allowreg:boolean):ttempcreatenode; inline;
    function ctempcreatenode_withnode(_typedef: tdef; _size: tcgint; _temptype: ttemptype; allowreg:boolean; withnode: tnode):ttempcreatenode; inline;
    function ctempcreatenode_value(_typedef:tdef; _size: tcgint; _temptype: ttemptype;allowreg:boolean; templvalue: tnode):ttempcreatenode; inline;
    function ctempcreatenode_reference(_typedef:tdef; _size: tcgint; _temptype: ttemptype;allowreg:boolean; templvalue: tnode; readonly: boolean):ttempcreatenode; inline;
    function ctemprefnode(const temp: ttempcreatenode):ttemprefnode; inline;
    function ctempdeletenode(const temp: ttempcreatenode):ttempdeletenode; inline;
    function ctempdeletenode_normal_temp(const temp: ttempcreatenode):ttempdeletenode; inline;
    { ncal }
    function ccallnode(l:tnode; v : tprocsym;st : TSymtable; mp: tnode; callflags:tcallnodeflags;sc:tspecializationcontext):tcallnode; inline;
    function ccallnode_procvar(l,r:tnode):tcallnode; inline;
    function ccallnode_intern(const name: string; params: tnode):tcallnode; inline;
    function ccallnode_fromintrinsic(const intrinsic: TInlineNumber; const name: string; params: tnode):tcallnode; inline;
    function ccallnode_internfromunit(const fromunit, procname: string; params: tnode):tcallnode; inline;
    function ccallnode_internres(const name: string; params: tnode; res:tdef):tcallnode; inline;
    function ccallnode_internresfromunit(const fromunit, procname: string; params: tnode; res:tdef):tcallnode; inline;
    function ccallnode_internreturn(const name: string; params: tnode; returnnode : tnode):tcallnode; inline;
    function ccallnode_internmethod(mp: tnode; const name: string; params: tnode):tcallnode; inline;
    function ccallnode_internmethodres(mp: tnode; const name: string; params: tnode; res:tdef):tcallnode; inline;
    function ccallparanode(expr,next : tnode):tcallparanode; inline;
    { ncnv }
    function ctypeconvnode(node : tnode;def:tdef):ttypeconvnode; inline;
    function ctypeconvnode_explicit(node : tnode;def:tdef):ttypeconvnode; inline;
    function ctypeconvnode_internal(node : tnode;def:tdef):ttypeconvnode; inline;
    function ctypeconvnode_proc_to_procvar(node : tnode):ttypeconvnode; inline;
    function casnode(l,r : tnode):tasnode; inline;
    function casnode_internal(l,r : tnode):tasnode; inline;
    function cisnode(l,r : tnode):tisnode; inline;
    function cisnode_internal(l,r : tnode):tisnode; inline;
    { ncon }
    function crealconstnode(v : bestreal;def:tdef):trealconstnode; inline;
    function cordconstnode(const v : tconstexprint;def:tdef; _rangecheck : boolean):tordconstnode; inline;
    function cpointerconstnode(v : TConstPtrUInt;def:tdef):tpointerconstnode; inline;
    function cstringconstnode_str(const s : string):tstringconstnode; inline;
    function cstringconstnode_pchar(s: pchar; l: longint; def: tdef):tstringconstnode; inline;
    function cstringconstnode_unistr(w : tcompilerwidestring):tstringconstnode; inline;
    function csetconstnode(s : pconstset;def:tdef):tsetconstnode; inline;
    function cguidconstnode(const g:tguid):tguidconstnode; inline;
    function cnilnode:tnilnode; inline;
    { nflw }
    function cwhilerepeatnode(l,r:Tnode;tab,cn:boolean):twhilerepeatnode; inline;
    function cifnode(l,r,_t1 : tnode):tifnode; inline;
    function cifnode_internal(l,r,_t1 : tnode):tifnode; inline;
    function cfornode(l,r,_t1,_t2 : tnode;back : boolean):tfornode; inline;
    function cexitnode(l:tnode):texitnode; inline;
    function cgotonode(p : tlabelsym):tgotonode; inline;
    function clabelnode(l:tnode;alabsym:tlabelsym):tlabelnode; inline;
    function craisenode(l,taddr,tframe:tnode):traisenode; inline;
    function ctryexceptnode(l,r,_t1 : tnode):ttryexceptnode; inline;
    function ctryfinallynode(l,r:tnode):ttryfinallynode; inline;
    function ctryfinallynode_implicit(l,r:tnode):ttryfinallynode; inline;
    function connode(l,r:tnode):tonnode; inline;
    function cbreaknode:tbreaknode; inline;
    function ccontinuenode:tcontinuenode; inline;
    { ninl }
    function cinlinenode(number : tinlinenumber;is_const:boolean;l : tnode):tinlinenode; inline;
    function cinlinenode_intern(number : tinlinenumber;is_const:boolean;l : tnode):tinlinenode; inline;
    { nld }
    function cloadnode(v : tsym;st : TSymtable):tloadnode; inline;
    function cloadnode_procvar(v : tsym;d:tprocdef;st : TSymtable):tloadnode; inline;
    function cassignmentnode(l,r : tnode):tassignmentnode; inline;
    function cassignmentnode_internal(l,r : tnode):tassignmentnode; inline;
    function carrayconstructorrangenode(l,r : tnode):tarrayconstructorrangenode; inline;
    function carrayconstructornode(l,r : tnode):tarrayconstructornode; inline;
    function ctypenode(def:tdef):ttypenode; inline;
    function crttinode(def:tstoreddef;rt:trttitype;dt:Trttidatatype):trttinode; inline;
    { nmat }
    function cmoddivnode(t:tnodetype;l,r : tnode):tmoddivnode; inline;
    function cshlshrnode(t:tnodetype;l,r : tnode):tshlshrnode; inline;
    function cunaryminusnode(expr : tnode):tunaryminusnode; inline;
    function cunaryplusnode(expr : tnode):tunaryplusnode; inline;
    function cnotnode(expr : tnode):tnotnode; inline;
    { nmem }
    function cloadvmtaddrnode(l : tnode):tloadvmtaddrnode; inline;
    function caddrnode(l : tnode):taddrnode; inline;
    function caddrnode_internal(l : tnode):taddrnode; inline;
    function caddrnode_internal_nomark(l : tnode):taddrnode; inline;
    function cderefnode(l : tnode):tderefnode; inline;
    function csubscriptnode(varsym : tsym;l : tnode):tsubscriptnode; inline;
    function cvecnode(l,r : tnode):tvecnode; inline;
    function cloadparentfpnode(pd: tprocdef; fpkind: tloadparentfpkind):tloadparentfpnode; inline;
    { nobjc }
    function cobjcselectornode(formethod: tnode):tobjcselectornode; inline;
    function cobjcprotocolnode(forprotocol: tnode):tobjcprotocolnode; inline;
    { nopt }
    function caddsstringcharoptnode(l,r : tnode):taddsstringcharoptnode; inline;
    function caddsstringcsstringoptnode(l,r : tnode):taddsstringcsstringoptnode; inline;
    { nset }
    function csetelementnode(l,r : tnode):tsetelementnode; inline;
    function cinnode(l,r : tnode):tinnode; inline;
    function crangenode(l,r : tnode):trangenode; inline;
    function ccasenode(l:tnode):tcasenode; inline;

    property Target: TCompilerTarget read GetTarget;
    property Verbose: TVerbose read GetVerbose;
    property Time: TCompilerTime read GetTime;
    property Globals: TCompilerGlobals read GetGlobals;
    property Parser: TParser read GetParser;
    property NodeUtils: TNodeUtils read GetNodeUtils;
    property Opt: TOptimizers read GetOpt;
    property ObjCGUtl: TObjCCodeGenUtils read GetObjCGUtl;
    property BlockUtl: TBlockUtils read GetBlockUtl;
    property PkgUtil: TPackageUtils read GetPkgUtil;
    property ProcDefUtil: TProcDefUtils read GetProcDefUtil;
    property ObjCUtil: TObjectiveCUtils read GetObjCUtil;
    property RTTIWriter : TRTTIWriter read GetRTTIWriter;
    property Linker: TLinker read GetLinker;
    property ExceptionStateHandler: tcgexceptionstatehandler read GetExceptionStateHandler;
    property hlcg: thlcgobj read GetHLCG;
    property cg: tcg read GetCG;
{$ifdef cpu64bitalu}
    property cg128 : tcg128 read GetCG128;
{$else cpu64bitalu}
    property cg64 : tcg64 read GetCG64;
{$endif cpu64bitalu}
    property paramanager: tparamanager read GetParaManager;
    property initialmacrosymtable: TSymtable read Getinitialmacrosymtable;
    property macrosymtablestack: TSymtablestack read Getmacrosymtablestack;
    property symtablestack: TSymtablestack read Getsymtablestack;
    property syssymlist: tsyssymlist read GetSysSymList;
    property aktassignmentnode : tassignmentnode read Getaktassignmentnode write Setaktassignmentnode;
  end;

function Compile(const cmd:TCmdStr):longint;

implementation

uses
  finput,
  fppu,
  aasmcpu;

{$if defined(MEMDEBUG)}
  {$define SHOWUSEDMEM}
{$endif}


{****************************************************************************
                                TCompiler
****************************************************************************}

procedure TCompiler.DoneCompiler;
begin
  if not CompilerInited then
   exit;
{ Free compiler if args are read }
  if CompilerInitedAfterArgs then
   begin
     CompilerInitedAfterArgs:=false;
     FreeAndNil(FParser);
     DoneImport;
     DoneExport;
     DoneLinker;
     DoneAsm;
     DoneWpo;
   end;
{ Free memory for the others }
  CompilerInited:=false;
  do_doneSymbolInfo;
  DoneSymtable(Self);
  FreeAndNil(FSysSymList);
  FreeAndNil(FGlobals);
  DoneFileUtils;
  donetokens;
  DoneTaskHandler(FTaskHandler);
  FreeAndNil(Fparamanager);
  FreeAndNil(FExceptionStateHandler);
  FreeAndNil(FTarget);
  FreeAndNil(FOpt);
  FreeAndNil(FOptions);
  FreeAndNil(FObjCUtil);
  FreeAndNil(FProcDefUtil);
  FreeAndNil(FPkgUtil);
  FreeAndNil(FBlockUtl);
  FreeAndNil(FObjCGUtl);
  FreeAndNil(FNodeUtils);
  FreeAndNil(FTime);
end;


procedure TCompiler.InitCompiler(const cmd:TCmdStr);
begin
  if CompilerInited then
   DoneCompiler;
{$if defined(unix)}
  { Set default code page for ansistrings on unix-like systems }
  DefaultSystemCodePage:=GetSystemCodePage;
{$endif}
  FNodeUtils:=tnodeutils.Create(Self);
  FObjCGUtl:=TObjCCodeGenUtils.Create(Self);
  FBlockUtl:=TBlockUtils.Create(Self);
  FPkgUtil:=TPackageUtils.Create(Self);
  FProcDefUtil:=TProcDefUtils.Create(Self);
  FObjCUtil:=TObjectiveCUtils.Create(Self);
  FOptions:=TOptions.Create(Self);
  FOpt:=TOptimizers.Create(Self);
  CreateExceptionStateHandler(tcgexceptionstatehandler);
  Fparamanager:=tcpuparamanager.Create(Self);
{ inits which need to be done before the arguments are parsed }
  FTarget:=TCompilerTarget.Create;
  { fileutils depends on source_info so it must be after systems }
  InitFileUtils;
  { globals depends on source_info so it must be after systems }
  FGlobals:=TCompilerGlobals.Create;
  FSysSymList:=tsyssymlist.Create;
  { verbose depends on exe_path and must be after globals }
  FVerbose:=TVerbose.Create;
  inittokens;
  InitSymtable(Self); {Must come before read_arguments, to enable macrosymstack}
  do_initSymbolInfo;
  CompilerInited:=true;
{ this is needed here for the IDE
  in case of compilation failure
  at the previous compile }
  set_current_module(nil);
{ read the arguments }
  options.read_arguments(cmd);
{ inits which depend on arguments }
  FParser:=TParser.Create(self);
  InitImport;
  InitExport;
  InitLinker;
  InitAsm;
  InitWpo;

  FTaskHandler:=InitTaskHandler(self);
  CompilerInitedAfterArgs:=true;
end;


function TCompiler.Compile(const cmd:TCmdStr):longint;

{$maxfpuregisters 0}

  procedure writecmdstrlist(w:longint;l:TCmdStrList);
  var
    hp : TCmdStrListItem;
  begin
    hp:=TCmdStrListItem(l.first);
    while assigned(hp) do
     begin
       verbose.Message1(w,hp.str);
       hp:=TCmdStrListItem(hp.next);
     end;
  end;

var
  timestr    : string[20];
  linkstr    : string[64];
{$ifdef SHOWUSEDMEM}
  hstatus : TFPCHeapStatus;
{$endif SHOWUSEDMEM}
  ExceptionMask : TFPUExceptionMask;
  totaltime : real;
  m : tppumodule;

begin
  current_compiler:=Self;
  m:=nil;
  try
    try
       ExceptionMask:=GetExceptionMask;
       SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                         exOverflow, exUnderflow, exPrecision]);

       FreeAndNil(FTime);
       FTime:=TCompilerTime.Create;
       GetLocalTime(Time.startsystime);
       Time.starttime := Time.getrealtime(Time.startsystime);

       { Initialize the compiler }
       InitCompiler(cmd);

       { apply global messages/verbosity }
       flushpendingswitchesstate;
       verbose.FreeLocalVerbosity(current_settings.pmessage);

       { show some info }
       verbose.Message1(general_t_compilername,FixFileName(system.paramstr(0)));
       verbose.Message1(general_d_sourceos,source_info.name);
       verbose.Message1(general_i_targetos,target.info.name);
       verbose.Message1(general_t_exepath,globals.exepath);
       WriteCmdStrList(general_t_unitpath,globals.unitsearchpath);
       WriteCmdStrList(general_t_includepath,globals.includesearchpath);
       WriteCmdStrList(general_t_librarypath,globals.librarysearchpath);
       WriteCmdStrList(general_t_objectpath,globals.objectsearchpath);
       WriteCmdStrList(general_t_unitscope,globals.namespacelist);

       { Compile the program }
  {$ifdef PREPROCWRITE}
       if parapreprocess then
        parser.preprocess(inputfilepath+inputfilename)
       else
  {$endif PREPROCWRITE}
         begin
         m:=tppumodule.create(Nil,'',Globals.inputfilepath+Globals.inputfilename,false,self);
         m.state:=ms_compile;
         m.is_initial:=true;
         { We need to add the initial module manually to the list of units }
         addloadedunit(m);
         main_module:=m;
         FTaskHandler.addmodule(m);
         FTaskHandler.processqueue;
         end;


       { Show statistics }
       if status.errorcount=0 then
        begin
          totaltime:=Time.getrealtime-Time.starttime;
          if totaltime<0 then
            totaltime:=totaltime+3600.0*24.0;
          if round(frac(totaltime)*10) >= 10 then
            totaltime:=trunc(totaltime) + 1;
          timestr:=tostr(trunc(totaltime))+'.'+tostr(round(frac(totaltime)*10));
          if status.codesize<>aword(-1) then
            linkstr:=', '+tostr(status.codesize)+' ' +verbose.MessageStr(general_text_bytes_code)+', '+tostr(status.datasize)+' '+verbose.MessageStr(general_text_bytes_data)
          else
            linkstr:='';
          verbose.Message3(general_i_abslines_compiled,tostr(status.compiledlines),timestr,linkstr);
          if (Status.Verbosity and V_Warning = V_Warning) and
                                               (Status.CountWarnings <> 0) then
           verbose.Message1 (general_i_number_of_warnings, tostr (Status.CountWarnings));
          if (Status.Verbosity and V_Hint = V_Hint) and
                                                  (Status.CountHints <> 0) then
           verbose.Message1 (general_i_number_of_hints, tostr (Status.CountHints));
          if (Status.Verbosity and V_Note = V_Note) and
                                               (Status.CountNotes <> 0) then
           verbose.Message1 (general_i_number_of_notes, tostr (Status.CountNotes));
        end;
     finally
       { no message possible after this !!    }
       DoneCompiler;

       SetExceptionMask(ExceptionMask);
     end;
     FreeAndNil(FVerbose);
  except
    on EControlCAbort do
      begin
        try
          { in case of 50 errors, this could cause another exception,
            suppress this exception
          }
          if assigned(m) then
            verbose.Message(general_f_compilation_aborted)
          else
            verbose.Message(general_f_compiler_aborted);
        except
          on ECompilerAbort do
            ;
        end;
        FreeAndNil(FVerbose);
      end;
    on ECompilerAbort do
      begin
        try
          { in case of 50 errors, this could cause another exception,
            suppress this exception
          }
{$ifdef DUMP_EXCEPTION_BACKTRACE}
          DumpExceptionBackTrace(stderr);
{$endif DUMP_EXCEPTION_BACKTRACE}
          verbose.Message(general_f_compilation_aborted);
        except
          on ECompilerAbort do
            ;
        end;
        FreeAndNil(FVerbose);
      end;
    on ECompilerAbortSilent do
      begin
        FreeAndNil(FVerbose);
      end;
    on EOutOfMemory do
      begin
        try
          verbose.Message(general_f_no_memory_left);
        except
          on ECompilerAbort do
            ;
        end;
        FreeAndNil(FVerbose);
      end;
    on e : EInOutError do
      begin
        try
          verbose.Message1(general_f_ioerror,e.Message);
        except
          on ECompilerAbort do
            ;
        end;
        FreeAndNil(FVerbose);
      end;
    on e : EOSError do
      begin
        try
          verbose.Message1(general_f_oserror,e.Message);
        except
          on ECompilerAbort do
            ;
        end;
        FreeAndNil(FVerbose);
      end;
    on Exception do
      begin
        { General catchall, normally not used }
        try
          { in case of 50 errors, this could cause another exception,
            suppress this exception
          }
          if not globals.exception_raised then
            begin
              globals.exception_raised:=true;
              verbose.Message(general_e_exception_raised);
            end
          else
            verbose.Message(general_f_compilation_aborted);
        except
          on ECompilerAbort do
            ;
        end;
        FreeAndNil(FVerbose);
        Raise;
      end;
  end;
{$ifdef SHOWUSEDMEM}
      hstatus:=GetFPCHeapStatus;
      Writeln('Max Memory used/heapsize: ',DStr(hstatus.MaxHeapUsed shr 10),'/',DStr(hstatus.MaxHeapSize shr 10),' Kb');
{$endif SHOWUSEDMEM}

  { Set the return value if an error has occurred }
  if status.errorcount=0 then
    result:=0
  else
    result:=1;
end;

procedure TCompiler.CreateExceptionStateHandler(eshclass: tcgexceptionstatehandlerclass);
begin
  FreeAndNil(FExceptionStateHandler);
  FExceptionStateHandler:=eshclass.create(Self);
end;

procedure TCompiler.InitLinker;
begin
  FreeAndNil(FLinker);
  FLinker:=CreateLinker(Self);
end;

procedure TCompiler.DoneLinker;
begin
  FreeAndNil(FLinker);
end;

{ TCompilerHelper }

function TCompilerHelper.Getaktassignmentnode: tassignmentnode; inline;
begin
  Result := TCompiler(Self).aktassignmentnode;
end;

function TCompilerHelper.GetBlockUtl: TBlockUtils; inline;
begin
  Result := TCompiler(Self).BlockUtl;
end;

function TCompilerHelper.GetCG: tcg; inline;
begin
  Result := TCompiler(Self).cg;
end;

{$ifdef cpu64bitalu}
function TCompilerHelper.GetCG128 : tcg128; inline;
begin
  Result := TCompiler(Self).cg128;
end;
{$else cpu64bitalu}
function TCompilerHelper.GetCG64 : tcg64; inline;
begin
  Result := TCompiler(Self).cg64;
end;
{$endif cpu64bitalu}

function TCompilerHelper.GetExceptionStateHandler: tcgexceptionstatehandler; inline;
begin
  Result := TCompiler(Self).ExceptionStateHandler;
end;

function TCompilerHelper.GetGlobals: TCompilerGlobals; inline;
begin
  Result := TCompiler(Self).Globals;
end;

function TCompilerHelper.GetHLCG: thlcgobj;
begin
  Result := TCompiler(Self).hlcg;
end;

function TCompilerHelper.Getinitialmacrosymtable: TSymtable; inline;
begin
  Result := TCompiler(Self).initialmacrosymtable;
end;

function TCompilerHelper.GetLinker: TLinker;
begin
  Result := TCompiler(Self).Linker;
end;

function TCompilerHelper.Getmacrosymtablestack: TSymtablestack;
begin
  Result := TCompiler(Self).macrosymtablestack;
end;

function TCompilerHelper.GetObjCGUtl: TObjCCodeGenUtils; inline;
begin
  Result := TCompiler(Self).ObjCGUtl;
end;

function TCompilerHelper.GetObjCUtil: TObjectiveCUtils; inline;
begin
  Result := TCompiler(Self).ObjCUtil;
end;

function TCompilerHelper.GetParaManager: tparamanager; inline;
begin
  Result := TCompiler(Self).paramanager;
end;

function TCompilerHelper.GetParser: TParser; inline;
begin
  Result := TCompiler(Self).Parser;
end;

function TCompilerHelper.GetNodeUtils: TNodeUtils;
begin
  Result := TCompiler(Self).NodeUtils;
end;

function TCompilerHelper.GetOpt: TOptimizers;
begin
  Result := TCompiler(Self).Opt;
end;

function TCompilerHelper.GetPkgUtil: TPackageUtils; inline;
begin
  Result := TCompiler(Self).PkgUtil;
end;

function TCompilerHelper.GetProcDefUtil: TProcDefUtils; inline;
begin
  Result := TCompiler(Self).ProcDefUtil;
end;

function TCompilerHelper.GetRTTIWriter: TRTTIWriter; inline;
begin
  Result := TCompiler(Self).RTTIWriter;
end;

function TCompilerHelper.Getsymtablestack: TSymtablestack;
begin
  Result := TCompiler(Self).symtablestack;
end;

function TCompilerHelper.GetSysSymList: tsyssymlist; inline;
begin
  Result := TCompiler(Self).SysSymList;
end;

function TCompilerHelper.GetTarget: TCompilerTarget; inline;
begin
  Result := TCompiler(Self).Target;
end;

function TCompilerHelper.GetTime: TCompilerTime;
begin
  Result := TCompiler(Self).Time;
end;

function TCompilerHelper.GetVerbose: TVerbose; inline;
begin
  Result := TCompiler(Self).Verbose;
end;

procedure TCompilerHelper.Setaktassignmentnode(AValue: tassignmentnode); inline;
begin
  TCompiler(Self).aktassignmentnode := AValue;
end;

function TCompilerHelper.caddnode(tt: tnodetype; l, r: tnode): taddnode; inline;
begin
  result:=nadd.caddnode.create(tt,l,r,self);
end;

function TCompilerHelper.caddnode_internal(tt: tnodetype; l, r: tnode): taddnode; inline;
begin
  result:=nadd.caddnode.create_internal(tt,l,r,self);
end;

function TCompilerHelper.cnothingnode: tnothingnode; inline;
begin
  result:=nbas.cnothingnode.create(self);
end;

function TCompilerHelper.cerrornode: terrornode; inline;
begin
  result:=nbas.cerrornode.create(self);
end;

function TCompilerHelper.cspecializenode(l: tnode; g: boolean; s: tsym; u: boolean): tspecializenode; inline;
begin
  result:=nbas.cspecializenode.create(l,g,s,u,self);
end;

function TCompilerHelper.cspecializenode_inherited(l: tnode; g: boolean;
  s: tsym; i: tdef): tspecializenode; inline;
begin
  result:=nbas.cspecializenode.create_inherited(l,g,s,i,self);
end;

function TCompilerHelper.cfinalizetempsnode: tfinalizetempsnode; inline;
begin
  result:=nbas.cfinalizetempsnode.create(self);
end;

function TCompilerHelper.casmnode(p: TAsmList): tasmnode; inline;
begin
  result:=nbas.casmnode.create(p,self);
end;

function TCompilerHelper.casmnode_get_position: tasmnode; inline;
begin
  result:=nbas.casmnode.create_get_position(self);
end;

function TCompilerHelper.cstatementnode(l, r: tnode): tstatementnode; inline;
begin
  result:=nbas.cstatementnode.create(l,r,self);
end;

function TCompilerHelper.cblocknode(l: tnode): tblocknode; inline;
begin
  result:=nbas.cblocknode.create(l,self);
end;

function TCompilerHelper.ctempcreatenode(_typedef: tdef; _size: tcgint;
  _temptype: ttemptype; allowreg: boolean): ttempcreatenode; inline;
begin
  result:=nbas.ctempcreatenode.create(_typedef,_size,_temptype,allowreg,self);
end;

function TCompilerHelper.ctempcreatenode_withnode(_typedef: tdef;
  _size: tcgint; _temptype: ttemptype; allowreg: boolean; withnode: tnode
  ): ttempcreatenode; inline;
begin
  result:=nbas.ctempcreatenode.create_withnode(_typedef,_size,_temptype,
    allowreg,withnode,self);
end;

function TCompilerHelper.ctempcreatenode_value(_typedef: tdef; _size: tcgint;
  _temptype: ttemptype; allowreg: boolean; templvalue: tnode): ttempcreatenode; inline;
begin
  result:=nbas.ctempcreatenode.create_value(_typedef,_size,_temptype,allowreg,
    templvalue,self);
end;

function TCompilerHelper.ctempcreatenode_reference(_typedef: tdef;
  _size: tcgint; _temptype: ttemptype; allowreg: boolean; templvalue: tnode;
  readonly: boolean): ttempcreatenode; inline;
begin
  result:=nbas.ctempcreatenode.create_reference(_typedef,_size,_temptype,
    allowreg,templvalue,readonly,self);
end;

function TCompilerHelper.ctemprefnode(const temp: ttempcreatenode
  ): ttemprefnode; inline;
begin
  result:=nbas.ctemprefnode.create(temp,self);
end;

function TCompilerHelper.ctempdeletenode(const temp: ttempcreatenode
  ): ttempdeletenode; inline;
begin
  result:=nbas.ctempdeletenode.create(temp,self);
end;

function TCompilerHelper.ctempdeletenode_normal_temp(const temp: ttempcreatenode
  ): ttempdeletenode; inline;
begin
  result:=nbas.ctempdeletenode.create_normal_temp(temp,self);
end;

function TCompilerHelper.ccallnode(l: tnode; v: tprocsym; st: TSymtable;
  mp: tnode; callflags: tcallnodeflags; sc: tspecializationcontext): tcallnode; inline;
begin
  result:=ncal.ccallnode.create(l,v,st,mp,callflags,sc,self);
end;

function TCompilerHelper.ccallnode_procvar(l, r: tnode): tcallnode; inline;
begin
  result:=ncal.ccallnode.create_procvar(l,r,self);
end;

function TCompilerHelper.ccallnode_intern(const name: string; params: tnode
  ): tcallnode; inline;
begin
  result:=ncal.ccallnode.createintern(name,params,self);
end;

function TCompilerHelper.ccallnode_fromintrinsic(
  const intrinsic: TInlineNumber; const name: string; params: tnode): tcallnode; inline;
begin
  result:=ncal.ccallnode.createfromintrinsic(intrinsic,name,params,self);
end;

function TCompilerHelper.ccallnode_internfromunit(const fromunit,
  procname: string; params: tnode): tcallnode; inline;
begin
  result:=ncal.ccallnode.createinternfromunit(fromunit,procname,params,self);
end;

function TCompilerHelper.ccallnode_internres(const name: string; params: tnode;
  res: tdef): tcallnode; inline;
begin
  result:=ncal.ccallnode.createinternres(name,params,res,self);
end;

function TCompilerHelper.ccallnode_internresfromunit(const fromunit,
  procname: string; params: tnode; res: tdef): tcallnode; inline;
begin
  result:=ncal.ccallnode.createinternresfromunit(fromunit,procname,params,res,self);
end;

function TCompilerHelper.ccallnode_internreturn(const name: string;
  params: tnode; returnnode: tnode): tcallnode; inline;
begin
  result:=ncal.ccallnode.createinternreturn(name,params,returnnode,self);
end;

function TCompilerHelper.ccallnode_internmethod(mp: tnode; const name: string;
  params: tnode): tcallnode; inline;
begin
  result:=ncal.ccallnode.createinternmethod(mp,name,params,self);
end;

function TCompilerHelper.ccallnode_internmethodres(mp: tnode;
  const name: string; params: tnode; res: tdef): tcallnode; inline;
begin
  result:=ncal.ccallnode.createinternmethodres(mp,name,params,res,self);
end;

function TCompilerHelper.ccallparanode(expr, next: tnode): tcallparanode; inline;
begin
  result:=ncal.ccallparanode.create(expr,next,self);
end;

function TCompilerHelper.ctypeconvnode(node: tnode; def: tdef): ttypeconvnode; inline;
begin
  result:=ncnv.ctypeconvnode.create(node,def,self);
end;

function TCompilerHelper.ctypeconvnode_explicit(node: tnode; def: tdef
  ): ttypeconvnode; inline;
begin
  result:=ncnv.ctypeconvnode.create_explicit(node,def,self);
end;

function TCompilerHelper.ctypeconvnode_internal(node: tnode; def: tdef
  ): ttypeconvnode; inline;
begin
  result:=ncnv.ctypeconvnode.create_internal(node,def,self);
end;

function TCompilerHelper.ctypeconvnode_proc_to_procvar(node: tnode
  ): ttypeconvnode; inline;
begin
  result:=ncnv.ctypeconvnode.create_proc_to_procvar(node,self);
end;

function TCompilerHelper.casnode(l, r: tnode): tasnode; inline;
begin
  result:=ncnv.casnode.create(l,r,self);
end;

function TCompilerHelper.casnode_internal(l, r: tnode): tasnode; inline;
begin
  result:=ncnv.casnode.create_internal(l,r,self);
end;

function TCompilerHelper.cisnode(l, r: tnode): tisnode; inline;
begin
  result:=ncnv.cisnode.create(l,r,self);
end;

function TCompilerHelper.cisnode_internal(l, r: tnode): tisnode; inline;
begin
  result:=ncnv.cisnode.create_internal(l,r,self);
end;

function TCompilerHelper.crealconstnode(v: bestreal; def: tdef): trealconstnode; inline;
begin
  result:=ncon.crealconstnode.create(v,def,self);
end;

function TCompilerHelper.cordconstnode(const v: tconstexprint; def: tdef;
  _rangecheck: boolean): tordconstnode; inline;
begin
  result:=ncon.cordconstnode.create(v,def,_rangecheck,self);
end;

function TCompilerHelper.cpointerconstnode(v: TConstPtrUInt; def: tdef
  ): tpointerconstnode; inline;
begin
  result:=ncon.cpointerconstnode.create(v,def,self);
end;

function TCompilerHelper.cstringconstnode_str(const s: string
  ): tstringconstnode; inline;
begin
  result:=ncon.cstringconstnode.createstr(s,self);
end;

function TCompilerHelper.cstringconstnode_pchar(s: pchar; l: longint; def: tdef
  ): tstringconstnode; inline;
begin
  result:=ncon.cstringconstnode.createpchar(s,l,def,self);
end;

function TCompilerHelper.cstringconstnode_unistr(w: tcompilerwidestring
  ): tstringconstnode; inline;
begin
  result:=ncon.cstringconstnode.createunistr(w,self);
end;

function TCompilerHelper.csetconstnode(s: pconstset; def: tdef): tsetconstnode; inline;
begin
  result:=ncon.csetconstnode.create(s,def,self);
end;

function TCompilerHelper.cguidconstnode(const g: tguid): tguidconstnode; inline;
begin
  result:=ncon.cguidconstnode.create(g,self);
end;

function TCompilerHelper.cnilnode: tnilnode; inline;
begin
  result:=ncon.cnilnode.create(self);
end;

function TCompilerHelper.cwhilerepeatnode(l, r: Tnode; tab, cn: boolean
  ): twhilerepeatnode; inline;
begin
  result:=nflw.cwhilerepeatnode.create(l,r,tab,cn,self);
end;

function TCompilerHelper.cifnode(l, r, _t1: tnode): tifnode; inline;
begin
  result:=nflw.cifnode.create(l,r,_t1,self);
end;

function TCompilerHelper.cifnode_internal(l, r, _t1: tnode): tifnode; inline;
begin
  result:=nflw.cifnode.create_internal(l,r,_t1,self);
end;

function TCompilerHelper.cfornode(l, r, _t1, _t2: tnode; back: boolean
  ): tfornode; inline;
begin
  result:=nflw.cfornode.create(l,r,_t1,_t2,back,self);
end;

function TCompilerHelper.cexitnode(l: tnode): texitnode; inline;
begin
  result:=nflw.cexitnode.create(l,self);
end;

function TCompilerHelper.cgotonode(p: tlabelsym): tgotonode; inline;
begin
  result:=nflw.cgotonode.create(p,self);
end;

function TCompilerHelper.clabelnode(l: tnode; alabsym: tlabelsym): tlabelnode; inline;
begin
  result:=nflw.clabelnode.create(l,alabsym,self);
end;

function TCompilerHelper.craisenode(l, taddr, tframe: tnode): traisenode; inline;
begin
  result:=nflw.craisenode.create(l,taddr,tframe,self);
end;

function TCompilerHelper.ctryexceptnode(l, r, _t1: tnode): ttryexceptnode; inline;
begin
  result:=nflw.ctryexceptnode.create(l,r,_t1,self);
end;

function TCompilerHelper.ctryfinallynode(l, r: tnode): ttryfinallynode; inline;
begin
  result:=nflw.ctryfinallynode.create(l,r,self);
end;

function TCompilerHelper.ctryfinallynode_implicit(l, r: tnode): ttryfinallynode; inline;
begin
  result:=nflw.ctryfinallynode.create_implicit(l,r,self);
end;

function TCompilerHelper.connode(l, r: tnode): tonnode; inline;
begin
  result:=nflw.connode.create(l,r,self);
end;

function TCompilerHelper.cbreaknode: tbreaknode; inline;
begin
  result:=nflw.cbreaknode.create(self);
end;

function TCompilerHelper.ccontinuenode: tcontinuenode; inline;
begin
  result:=nflw.ccontinuenode.create(self);
end;

function TCompilerHelper.cinlinenode(number: tinlinenumber; is_const: boolean;
  l: tnode): tinlinenode; inline;
begin
  result:=ninl.cinlinenode.create(number,is_const,l,self);
end;

function TCompilerHelper.cinlinenode_intern(number: tinlinenumber;
  is_const: boolean; l: tnode): tinlinenode; inline;
begin
  result:=ninl.cinlinenode.createintern(number,is_const,l,self);
end;

function TCompilerHelper.cloadnode(v: tsym; st: TSymtable): tloadnode; inline;
begin
  result:=nld.cloadnode.create(v,st,self);
end;

function TCompilerHelper.cloadnode_procvar(v: tsym; d: tprocdef; st: TSymtable
  ): tloadnode; inline;
begin
  result:=nld.cloadnode.create_procvar(v,d,st,self);
end;

function TCompilerHelper.cassignmentnode(l, r: tnode): tassignmentnode; inline;
begin
  result:=nld.cassignmentnode.create(l,r,self);
end;

function TCompilerHelper.cassignmentnode_internal(l, r: tnode): tassignmentnode; inline;
begin
  result:=nld.cassignmentnode.create_internal(l,r,self);
end;

function TCompilerHelper.carrayconstructorrangenode(l, r: tnode
  ): tarrayconstructorrangenode; inline;
begin
  result:=nld.carrayconstructorrangenode.create(l,r,self);
end;

function TCompilerHelper.carrayconstructornode(l, r: tnode
  ): tarrayconstructornode; inline;
begin
  result:=nld.carrayconstructornode.create(l,r,self);
end;

function TCompilerHelper.ctypenode(def: tdef): ttypenode; inline;
begin
  result:=nld.ctypenode.create(def,self);
end;

function TCompilerHelper.crttinode(def: tstoreddef; rt: trttitype;
  dt: Trttidatatype): trttinode; inline;
begin
  result:=nld.crttinode.create(def,rt,dt,self);
end;

function TCompilerHelper.cmoddivnode(t: tnodetype; l, r: tnode): tmoddivnode; inline;
begin
  result:=nmat.cmoddivnode.create(t,l,r,self);
end;

function TCompilerHelper.cshlshrnode(t: tnodetype; l, r: tnode): tshlshrnode; inline;
begin
  result:=nmat.cshlshrnode.create(t,l,r,self);
end;

function TCompilerHelper.cunaryminusnode(expr: tnode): tunaryminusnode; inline;
begin
  result:=nmat.cunaryminusnode.create(expr,self);
end;

function TCompilerHelper.cunaryplusnode(expr: tnode): tunaryplusnode; inline;
begin
  result:=nmat.cunaryplusnode.create(expr,self);
end;

function TCompilerHelper.cnotnode(expr: tnode): tnotnode; inline;
begin
  result:=nmat.cnotnode.create(expr,self);
end;

function TCompilerHelper.cloadvmtaddrnode(l: tnode): tloadvmtaddrnode; inline;
begin
  result:=nmem.cloadvmtaddrnode.create(l,self);
end;

function TCompilerHelper.caddrnode(l: tnode): taddrnode; inline;
begin
  result:=nmem.caddrnode.create(l,self);
end;

function TCompilerHelper.caddrnode_internal(l: tnode): taddrnode; inline;
begin
  result:=nmem.caddrnode.create_internal(l,self);
end;

function TCompilerHelper.caddrnode_internal_nomark(l: tnode): taddrnode; inline;
begin
  result:=nmem.caddrnode.create_internal_nomark(l,self);
end;

function TCompilerHelper.cderefnode(l: tnode): tderefnode; inline;
begin
  result:=nmem.cderefnode.create(l,self);
end;

function TCompilerHelper.csubscriptnode(varsym: tsym; l: tnode): tsubscriptnode; inline;
begin
  result:=nmem.csubscriptnode.create(varsym,l,self);
end;

function TCompilerHelper.cvecnode(l, r: tnode): tvecnode; inline;
begin
  result:=nmem.cvecnode.create(l,r,self);
end;

function TCompilerHelper.cloadparentfpnode(pd: tprocdef;
  fpkind: tloadparentfpkind): tloadparentfpnode; inline;
begin
  result:=nmem.cloadparentfpnode.create(pd,fpkind,self);
end;

function TCompilerHelper.cobjcselectornode(formethod: tnode): tobjcselectornode; inline;
begin
  result:=nobjc.cobjcselectornode.create(formethod,self);
end;

function TCompilerHelper.cobjcprotocolnode(forprotocol: tnode
  ): tobjcprotocolnode; inline;
begin
  result:=nobjc.cobjcprotocolnode.create(forprotocol,self);
end;

function TCompilerHelper.caddsstringcharoptnode(l, r: tnode
  ): taddsstringcharoptnode; inline;
begin
  result:=nopt.caddsstringcharoptnode.create(l,r,self);
end;

function TCompilerHelper.caddsstringcsstringoptnode(l, r: tnode
  ): taddsstringcsstringoptnode; inline;
begin
  result:=nopt.caddsstringcsstringoptnode.create(l,r,self);
end;

function TCompilerHelper.csetelementnode(l, r: tnode): tsetelementnode; inline;
begin
  result:=nset.csetelementnode.create(l,r,self);
end;

function TCompilerHelper.cinnode(l, r: tnode): tinnode; inline;
begin
  result:=nset.cinnode.create(l,r,self);
end;

function TCompilerHelper.crangenode(l, r: tnode): trangenode; inline;
begin
  result:=nset.crangenode.create(l,r,self);
end;

function TCompilerHelper.ccasenode(l: tnode): tcasenode; inline;
begin
  result:=nset.ccasenode.create(l,self);
end;

function Compile(const cmd:TCmdStr):longint;
var
  Compiler: TCompiler;
begin
  Compiler:=TCompiler.Create;
  try
    Result:=Compiler.Compile(cmd);
  finally
    Compiler.Free;
  end;
end;

end.
