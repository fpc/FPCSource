{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Peter Vreman

    Reads command line options and config files

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
unit options;

{$i defines.inc}

interface

uses
  globtype,globals,verbose;

type
  TOption=class
    FirstPass,
    NoPressEnter,
    DoWriteLogo : boolean;
    FileLevel : longint;
    QuickInfo : string;
    ParaIncludePath,
    ParaUnitPath,
    ParaObjectPath,
    ParaLibraryPath : TSearchPathList;
    Constructor Create;
    Destructor Destroy;override;
    procedure WriteLogo;
    procedure WriteInfo;
    procedure WriteHelpPages;
    procedure WriteQuickInfo;
    procedure IllegalPara(const opt:string);
    function  Unsetbool(const opts:string; pos: Longint):boolean;
    procedure interpret_proc_specific_options(const opt:string);virtual;
    procedure interpret_option(const opt :string;ispara:boolean);
    procedure Interpret_envvar(const envname : string);
    procedure Interpret_file(const filename : string);
    procedure Read_Parameters;
    procedure parsecmd(cmd:string);
  end;

  TOptionClass=class of toption;

var
  coption : TOptionClass;

procedure read_arguments(cmd:string);


implementation

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  version,systems,
  cutils,messages
{$ifdef BrowserLog}
  ,browlog
{$endif BrowserLog}
  ;

const
  page_size = 24;

var
  option     : toption;
  read_configfile,        { read config file, set when a cfgfile is found }
  disable_configfile,
  target_is_set : boolean;  { do not allow contradictory target settings }
  asm_is_set  : boolean; { -T also change initoutputformat if not set idrectly }
  fpcdir,
  ppccfg,
  param_file    : string;   { file to compile specified on the commandline }

{****************************************************************************
                                 Defines
****************************************************************************}

procedure def_symbol(const s : string);
begin
  if s='' then
   exit;
  initdefines.insert(upper(s));
end;


procedure undef_symbol(const s : string);
begin
  if s='' then
   exit;
  InitDefines.Remove(s);
end;


function check_symbol(const s:string):boolean;
begin
  check_symbol:=(initdefines.find(s)<>nil);
end;


procedure set_default_link_type;
begin
  if (target_info.target=target_i386_win32) then
    begin
      def_symbol('FPC_LINK_SMART');
      undef_symbol('FPC_LINK_STATIC');
      undef_symbol('FPC_LINK_DYNAMIC');
      initglobalswitches:=initglobalswitches+[cs_link_smart];
      initglobalswitches:=initglobalswitches-[cs_link_shared,cs_link_static];
    end
  else
    begin
      undef_symbol('FPC_LINK_SMART');
      def_symbol('FPC_LINK_STATIC');
      undef_symbol('FPC_LINK_DYNAMIC');
      initglobalswitches:=initglobalswitches+[cs_link_static];
      initglobalswitches:=initglobalswitches-[cs_link_shared,cs_link_smart];
    end;
end;



{****************************************************************************
                                 Toption
****************************************************************************}

procedure StopOptions;
begin
  if assigned(Option) then
   begin
     Option.free;
     Option:=nil;
   end;
  DoneVerbose;
  Stop;
end;


procedure Toption.WriteLogo;
var
  p : pchar;
begin
  p:=MessagePchar(option_logo);
  while assigned(p) do
   Comment(V_Normal,GetMsgLine(p));
end;


procedure Toption.WriteInfo;
var
  p : pchar;
begin
  p:=MessagePchar(option_info);
  while assigned(p) do
   Comment(V_Normal,GetMsgLine(p));
  StopOptions;
end;


procedure Toption.WriteHelpPages;

  function PadEnd(s:string;i:longint):string;
  begin
    while (length(s)<i) do
     s:=s+' ';
    PadEnd:=s;
  end;

var
  lastident,
  j,outline,
  ident,
  lines : longint;
  show  : boolean;
  opt   : string[32];
  input,
  s     : string;
  p     : pchar;
begin
  WriteLogo;
  Lines:=4;
  Message1(option_usage,system.paramstr(0));
  lastident:=0;
  p:=MessagePChar(option_help_pages);
  while assigned(p) do
   begin
   { get a line and reset }
     s:=GetMsgLine(p);
     ident:=0;
     show:=false;
   { parse options }
     case s[1] of
{$ifdef UNITALIASES}
      'a',
{$endif}
{$ifdef EXTDEBUG}
      'e',
{$endif EXTDEBUG}
{$ifdef i386}
      '3',
{$endif}
{$ifdef m68k}
      '6',
{$endif}
      '*' : show:=true;
     end;
     if show then
      begin
        case s[2] of
{$ifdef GDB}
         'g',
{$endif}
{$ifdef Unix}
         'L',
{$endif}
{$ifdef os2}
         'O',
{$endif}
         '*' : show:=true;
        else
         show:=false;
        end;
      end;
   { now we may show the message or not }
     if show then
      begin
        case s[3] of
         '0' : begin
                 ident:=0;
                 outline:=0;
               end;
         '1' : begin
                 ident:=2;
                 outline:=7;
               end;
         '2' : begin
                 ident:=6;
                 outline:=11;
               end;
         '3' : begin
                 ident:=9;
                 outline:=6;
               end;
        end;
        j:=pos('_',s);
        opt:=Copy(s,4,j-4);
        if opt='*' then
         opt:=''
        else
        if opt=' ' then
         opt:=PadEnd(opt,outline)
        else
         opt:=PadEnd('-'+opt,outline);
        if (ident=0) and (lastident<>0) then
         begin
           Comment(V_Normal,'');
           inc(Lines);
         end;
      { page full ? }
        if (lines >= page_size - 1) then
         begin
           if not NoPressEnter then
            begin
              Message(option_help_press_enter);
              readln(input);
              if upper(input)='Q' then
               StopOptions;
            end;
           lines:=0;
         end;
        Comment(V_Normal,PadEnd('',ident)+opt+Copy(s,j+1,255));
        LastIdent:=Ident;
        inc(Lines);
      end;
   end;
  StopOptions;
end;


procedure Toption.IllegalPara(const opt:string);
begin
  Message1(option_illegal_para,opt);
  Message(option_help_pages_para);
  StopOptions;
end;


function Toption.Unsetbool(const opts:string; pos: Longint):boolean;
{ checks if the character after pos in Opts is a + or a - and returns resp.
  false or true. If it is another character (or none), it also returns false }
begin
  UnsetBool := (Length(Opts) > Pos) And (Opts[Succ(Pos)] = '-');
end;


procedure TOption.interpret_proc_specific_options(const opt:string);
begin
end;


procedure TOption.interpret_option(const opt:string;ispara:boolean);
var
  code : integer;
  c    : char;
  more : string;
  major,minor : longint;
  error : integer;
  j,l  : longint;
  d    : DirStr;
  e    : ExtStr;
  forceasm : tasm;
begin
  if opt='' then
   exit;

  { only parse define,undef,target,verbosity and link options the firsttime }
  if firstpass and
     not((opt[1]='-') and (opt[2] in ['i','d','v','T','u','n','X'])) then
   exit;

  Message1(option_handling_option,opt);
  case opt[1] of
 '-' : begin
         more:=Copy(opt,3,255);
         case opt[2] of
              '!' : initlocalswitches:=initlocalswitches+[cs_ansistrings];
              '?' : WriteHelpPages;
              'a' : begin
                      initglobalswitches:=initglobalswitches+[cs_asm_leave];
                      for j:=1 to length(more) do
                       case more[j] of
                        'l' : initglobalswitches:=initglobalswitches+[cs_asm_source];
                        'r' : initglobalswitches:=initglobalswitches+[cs_asm_regalloc];
                        't' : initglobalswitches:=initglobalswitches+[cs_asm_tempalloc];
                        '-' : initglobalswitches:=initglobalswitches-
                                [cs_asm_leave, cs_asm_source,cs_asm_regalloc, cs_asm_tempalloc];
                       else
                         IllegalPara(opt);
                       end;
                    end;
              'A' : begin
                      if set_target_asm_by_string(More) then
                       asm_is_set:=true
                      else
                       IllegalPara(opt);
                    end;
              'b' : begin
{$ifdef BrowserLog}
                      initglobalswitches:=initglobalswitches+[cs_browser_log];
{$endif}
                      if More<>'' then
                        if More='l' then
                          initmoduleswitches:=initmoduleswitches+[cs_local_browser]
                        else if More='-' then
                          begin
                            initmoduleswitches:=initmoduleswitches-[cs_browser,cs_local_browser];
{$ifdef BrowserLog}
                            initglobalswitches:=initglobalswitches-[cs_browser_log];
{$endif}
                          end
                        else if More<>'+' then
{$ifdef BrowserLog}
                          browserlog.elements_to_list.insert(more);
{$else}
                          IllegalPara(opt);
{$endif}
                    end;
              'B' : if UnSetBool(more,0) then
                      do_build:=false
                    else
                      do_build:=true;
              'C' : begin
                      j := 1;
                      while j <= length(more) Do
                        Begin
                          case more[j] of
                            'a' : Message2(option_obsolete_switch_use_new,'-Ca','-Or');
                            'h' :
                               begin
                                 val(copy(more,j+1,length(more)-j),heapsize,code);
                                 if (code<>0) or (heapsize>=67107840) or (heapsize<1024) then
                                  IllegalPara(opt);
                                 break;
                               end;
                            'i' : If UnsetBool(More, j) then
                                    Begin
                                      initlocalswitches:=initlocalswitches-[cs_check_io];
                                      inc(j)
                                    End
                                  else initlocalswitches:=initlocalswitches+[cs_check_io];
                            'n' : If UnsetBool(More, j) then
                                    Begin
                                      initglobalswitches:=initglobalswitches-[cs_link_extern];
                                      inc(j)
                                    End
                                  Else initglobalswitches:=initglobalswitches+[cs_link_extern];
                            'o' :
                              If UnsetBool(More, j) then
                                Begin
                                  initlocalswitches:=initlocalswitches-[cs_check_overflow];
                                  inc(j);
                                End
                              Else
                                initlocalswitches:=initlocalswitches+[cs_check_overflow];
                            'r' :
                              If UnsetBool(More, j) then
                                Begin
                                  initlocalswitches:=initlocalswitches-[cs_check_range];
                                  inc(j);
                                End
                              Else
                                initlocalswitches:=initlocalswitches+[cs_check_range];
                            'R' :
                              If UnsetBool(More, j) then
                                Begin
                                  initlocalswitches:=initlocalswitches-[cs_check_object_ext];
                                  inc(j);
                                End
                              Else
                                initlocalswitches:=initlocalswitches+[cs_check_object_ext];
                            's' :
                               begin
                                 val(copy(more,j+1,length(more)-j),stacksize,code);
                                 if (code<>0) or (stacksize>=67107840) or (stacksize<1024) then
                                  IllegalPara(opt);
                                 break;
                               end;
                            't' :
                               If UnsetBool(More, j) then
                                 Begin
                                   initlocalswitches:=initlocalswitches-[cs_check_stack];
                                   inc(j)
                                 End
                               Else
                                 initlocalswitches:=initlocalswitches+[cs_check_stack];
                            'D' :
                               If UnsetBool(More, j) then
                                 Begin
                                   initmoduleswitches:=initmoduleswitches-[cs_create_dynamic];
                                   inc(j)
                                 End
                               Else
                                 initmoduleswitches:=initmoduleswitches+[cs_create_dynamic];
                            'X' :
                               If UnsetBool(More, j) then
                                 Begin
                                   initmoduleswitches:=initmoduleswitches-[cs_create_smart];
                                   inc(j)
                                 End
                               Else
                                 initmoduleswitches:=initmoduleswitches+[cs_create_smart];
                            else
                               IllegalPara(opt);
                          end;
                          inc(j);
                        end;
                    end;
              'd' : def_symbol(more);
              'D' : begin
                      initglobalswitches:=initglobalswitches+[cs_link_deffile];
                      for j:=1 to length(more) do
                       case more[j] of
                        'd' : begin
                                description:=Copy(more,j+1,255);
                                break;
                              end;
                        'v' : begin
                                dllversion:=Copy(more,j+1,255);
                                l:=pos('.',dllversion);
                                dllminor:=0;
                                error:=0;
                                if l>0 then
                                  begin
                                    valint(copy(dllversion,l+1,255),minor,error);
                                    if (error=0) and
                                       (minor>=0) and (minor<=$ffff) then
                                      dllminor:=minor
                                    else if error=0 then
                                      error:=1;
                                  end;
                                if l=0 then l:=256;
                                dllmajor:=1;
                                if error=0 then
                                  valint(copy(dllversion,1,l-1),major,error);
                                if (error=0) and (major>=0) and (major<=$ffff) then
                                  dllmajor:=major
                                else if error=0 then
                                  error:=1;
                                if error<>0 then
                                  Message1(scan_w_wrong_version_ignored,dllversion);
                                break;
                              end;
                        'w' : usewindowapi:=true;
                        '-' : begin
                                initglobalswitches:=initglobalswitches-
                                  [cs_link_deffile];
                                usewindowapi:=false;
                              end;
                       else
                         IllegalPara(opt);
                       end;
                    end;
              'e' : exepath:=FixPath(More,true);
              { Just used by RHIDE }
              'E' : if (length(more)=0) or (UnsetBool(More, 0)) then
                      initglobalswitches:=initglobalswitches+[cs_link_extern]
                    else
                      initglobalswitches:=initglobalswitches-[cs_link_extern];
              'F' : begin
                      c:=more[1];
                      Delete(more,1,1);
                      DefaultReplacements(More);
                      case c of
                       'D' : utilsdirectory:=FixPath(More,true);
                       'e' : SetRedirectFile(More);
                       'E' : OutputExeDir:=FixPath(More,true);
                       'i' : if ispara then
                              ParaIncludePath.AddPath(More,false)
                             else
                              includesearchpath.AddPath(More,true);
                       'g' : Message2(option_obsolete_switch_use_new,'-Fg','-Fl');
                       'l' : if ispara then
                              ParaLibraryPath.AddPath(More,false)
                             else
                              LibrarySearchPath.AddPath(More,true);
                       'L' : if More<>'' then
                              ParaDynamicLinker:=More
                             else
                              IllegalPara(opt);
                       'o' : if ispara then
                              ParaObjectPath.AddPath(More,false)
                             else
                              ObjectSearchPath.AddPath(More,true);
                       'r' : Msgfilename:=More;
                       'u' : if ispara then
                              ParaUnitPath.AddPath(More,false)
                             else
                              unitsearchpath.AddPath(More,true);
                       'U' : OutputUnitDir:=FixPath(More,true);
                      else
                        IllegalPara(opt);
                      end;
                    end;
              'g' : begin
                      if UnsetBool(More, 0) then
                        begin
                          initmoduleswitches:=initmoduleswitches-[cs_debuginfo];
                          if (length(More)>1) and (More[2]='l') then
                            initglobalswitches:=initglobalswitches+[cs_gdb_lineinfo];
                        end
                      else
                       begin
{$ifdef GDB}
                         initmoduleswitches:=initmoduleswitches+[cs_debuginfo];
                         if not RelocSectionSetExplicitly then
                           RelocSection:=false;
                         for j:=1 to length(more) do
                          case more[j] of
                           'd' : initglobalswitches:=initglobalswitches+[cs_gdb_dbx];
                           'g' : initglobalswitches:=initglobalswitches+[cs_gdb_gsym];
                           'h' : initglobalswitches:=initglobalswitches+[cs_gdb_heaptrc];
                           'l' : initglobalswitches:=initglobalswitches+[cs_gdb_lineinfo];
                           'c' : initglobalswitches:=initglobalswitches+[cs_checkpointer];
{$ifdef EXTDEBUG}
                           'p' : only_one_pass:=true;
{$endif EXTDEBUG}
                          else
                            IllegalPara(opt);
                          end;
{$else GDB}
                         Message(option_no_debug_support);
                         Message(option_no_debug_support_recompile_fpc);
{$endif GDB}
                       end;
                    end;
              'h' : begin
                      NoPressEnter:=true;
                      WriteHelpPages;
                    end;
              'i' : if More='' then
                     WriteInfo
                    else
                     QuickInfo:=QuickInfo+More;
              'I' : if ispara then
                     ParaIncludePath.AddPath(More,false)
                    else
                     includesearchpath.AddPath(More,false);
              'k' : if more<>'' then
                     ParaLinkOptions:=ParaLinkOptions+' '+More
                    else
                     IllegalPara(opt);
              'l' : if UnSetBool(more,0) then
                      DoWriteLogo:=false
                    else
                      DoWriteLogo:=true;
              'm' : if UnSetBool(more,0) then
                      parapreprocess:=false
                    else
                      parapreprocess:=true;
              'n' : if More='' then
                     begin
                       read_configfile:=false;
                       disable_configfile:=true;
                     end
                    else
                     IllegalPara(opt);
              'o' : if More<>'' then
                     Fsplit(More,d,OutputFile,e)
                    else
                     IllegalPara(opt);
              'p' : begin
                      if UnsetBool(More, 0) then
                        begin
                          initmoduleswitches:=initmoduleswitches-[cs_profile];
                          undef_symbol('FPC_PROFILE');
                        end
                      else
                        case more[1] of
                         'g' : if (length(opt)=3) and UnsetBool(more, 1) then
                                begin
                                  initmoduleswitches:=initmoduleswitches-[cs_profile];
                                  undef_symbol('FPC_PROFILE');
                                end
                               else
                                begin
                                  initmoduleswitches:=initmoduleswitches+[cs_profile];
                                  def_symbol('FPC_PROFILE');
                               end;
                        else
                          IllegalPara(opt);
                        end;
                    end;
{$ifdef Unix}
              'P' : if UnsetBool(More, 0) then
                      initglobalswitches:=initglobalswitches-[cs_asm_pipe]
                    else
                      initglobalswitches:=initglobalswitches+[cs_asm_pipe];
{$endif Unix}
              's' : if UnsetBool(More, 0) then
                      initglobalswitches:=initglobalswitches-[cs_asm_extern,cs_link_extern]
                    else
                      initglobalswitches:=initglobalswitches+[cs_asm_extern,cs_link_extern];
              'S' : begin
                      if more[1]='I' then
                        begin
                          if upper(more)='ICOM' then
                            initinterfacetype:=it_interfacecom
                          else if upper(more)='ICORBA' then
                            initinterfacetype:=it_interfacecorba
                          else
                            IllegalPara(opt);
                        end
                      else
                        for j:=1 to length(more) do
                         case more[j] of
                          '2' : SetCompileMode('OBJFPC',true);
                          'a' : initlocalswitches:=InitLocalswitches+[cs_do_assertion];
                          'c' : initmoduleswitches:=initmoduleswitches+[cs_support_c_operators];
                          'd' : SetCompileMode('DELPHI',true);
                          'e' : begin
                                  SetErrorFlags(copy(more,j+1,length(more)));
                                  break;
                                end;
                          'g' : initmoduleswitches:=initmoduleswitches+[cs_support_goto];
                          'h' : initlocalswitches:=initlocalswitches+[cs_ansistrings];
                          'i' : initmoduleswitches:=initmoduleswitches+[cs_support_inline];
                          'm' : initmoduleswitches:=initmoduleswitches+[cs_support_macro];
                          'o' : SetCompileMode('TP',true);
                          'p' : SetCompileMode('GPC',true);
                          's' : initglobalswitches:=initglobalswitches+[cs_constructor_name];
                          't' : initmoduleswitches:=initmoduleswitches+[cs_static_keyword];
                          '-' : begin
                                  initglobalswitches:=initglobalswitches -
                                    [cs_constructor_name];
                                  initlocalswitches:=InitLocalswitches -
                                    [cs_do_assertion, cs_ansistrings];
                                  initmoduleswitches:=initmoduleswitches -
                                    [cs_support_c_operators, cs_support_goto,
                                     cs_support_inline, cs_support_macro,
                                     cs_static_keyword];
                                end;
                         else
                          IllegalPara(opt);
                         end;
                    end;
              'T' : begin
                      more:=Upper(More);
                      if not target_is_set then
                       begin
                         {Remove non core targetname extra defines}
                         case target_info.target of
                          target_i386_freebsd :
                            begin
                              undef_symbol('LINUX');
                              undef_symbol('BSD');
                              undef_symbol('UNIX');
                            end;
                          target_i386_linux :
                            begin
                              undef_symbol('UNIX');
                            end;
                         end;
                         { remove old target define }
                         undef_symbol(upper(target_info.shortname));
                         { Save assembler if set }
                         if asm_is_set then
                          forceasm:=target_asm.id;
                         { load new target }
                         if not(set_target_by_string(More)) then
                           IllegalPara(opt);
                         { also initialize assembler if not explicitly set }
                         if asm_is_set then
                          set_target_asm(forceasm);
                         { set new define }
                         def_symbol(upper(target_info.shortname));
                         target_is_set:=true;
                       end
                      else
                       if More<>upper(target_info.shortname) then
                        Message1(option_target_is_already_set,target_info.shortname);
                    end;
              'u' : undef_symbol(upper(More));
              'U' : begin
                      for j:=1 to length(more) do
                       case more[j] of
{$ifdef UNITALIASES}
                        'a' : begin
                                AddUnitAlias(Copy(More,j+1,255));
                                break;
                              end;
{$endif UNITALIASES}
                        'n' : initglobalswitches:=initglobalswitches-[cs_check_unit_name];
                        'p' : begin
                                Message2(option_obsolete_switch_use_new,'-Up','-Fu');
                                break;
                              end;
                        's' : initmoduleswitches:=initmoduleswitches+[cs_compilesystem];
                        '-' : begin
                                initmoduleswitches:=initmoduleswitches
                                  - [cs_compilesystem];
                                initglobalswitches:=initglobalswitches
                                  + [cs_check_unit_name];
                              end;
                       else
                         IllegalPara(opt);
                       end;
                    end;
              'v' : if not setverbosity(More) then
                     IllegalPara(opt);
              'W' : begin
                      for j:=1 to length(More) do
                       case More[j] of
                        'B': {bind_win32_dll:=true}
                             begin
                               {  -WB200000 means set trefered base address
                                 to $200000, but does not change relocsection boolean
                                 this way we can create both relocatble and
                                 non relocatable DLL at a specific base address PM }
                               if (length(More)>j) then
                                 begin
                                   if DLLImageBase=nil then
                                     DLLImageBase:=StringDup(Copy(More,j+1,255));
                                 end
                               else
                                 begin
                                   RelocSection:=true;
                                   RelocSectionSetExplicitly:=true;
                                 end;
                               break;
                             end;
                        'C': apptype:=app_cui;
                        'D': if UnsetBool(More, j) then
                              ForceDeffileForExport:=false
                             else
                              ForceDeffileForExport:=true;
                        'F': apptype:=app_fs;
                        'G': apptype:=app_gui;
                        'N': begin
                               RelocSection:=false;
                               RelocSectionSetExplicitly:=true;
                             end;
                        'R': begin
                               RelocSection:=true;
                               RelocSectionSetExplicitly:=true;
                             end;
                       else
                        IllegalPara(opt);
                       end;
                    end;
              'X' : begin
                      for j:=1 to length(More) do
                       case More[j] of
                        'c' : initglobalswitches:=initglobalswitches+[cs_link_toc];
                        's' : initglobalswitches:=initglobalswitches+[cs_link_strip];
                        't' : initglobalswitches:=initglobalswitches+[cs_link_staticflag];
                        'D' : begin
                                def_symbol('FPC_LINK_DYNAMIC');
                                undef_symbol('FPC_LINK_SMART');
                                undef_symbol('FPC_LINK_STATIC');
                                initglobalswitches:=initglobalswitches+[cs_link_shared];
                                initglobalswitches:=initglobalswitches-[cs_link_static,cs_link_smart];
                                LinkTypeSetExplicitly:=true;
                              end;
                        'S' : begin
                                def_symbol('FPC_LINK_STATIC');
                                undef_symbol('FPC_LINK_SMART');
                                undef_symbol('FPC_LINK_DYNAMIC');
                                initglobalswitches:=initglobalswitches+[cs_link_static];
                                initglobalswitches:=initglobalswitches-[cs_link_shared,cs_link_smart];
                                LinkTypeSetExplicitly:=true;
                              end;
                        'X' : begin
                                def_symbol('FPC_LINK_SMART');
                                undef_symbol('FPC_LINK_STATIC');
                                undef_symbol('FPC_LINK_DYNAMIC');
                                initglobalswitches:=initglobalswitches+[cs_link_smart];
                                initglobalswitches:=initglobalswitches-[cs_link_shared,cs_link_static];
                                LinkTypeSetExplicitly:=true;
                              end;
                        '-' : begin
                                initglobalswitches:=initglobalswitches-[cs_link_toc, cs_link_strip, cs_link_staticflag];
                                set_default_link_type;
                              end;
                       else
                         IllegalPara(opt);
                       end;
                    end;
       { give processor specific options a chance }
         else
          interpret_proc_specific_options(opt);
         end;
       end;
 '@' : begin
         Message(option_no_nested_response_file);
         StopOptions;
       end;
  else
   begin
     if (length(param_file)<>0) then
       Message(option_only_one_source_support);
     param_file:=opt;
   end;
  end;
end;


procedure Toption.Interpret_file(const filename : string);

  procedure RemoveSep(var fn:string);
  var
    i : longint;
  begin
    i:=0;
    while (i<length(fn)) and (fn[i+1] in [',',' ',#9]) do
      inc(i);
    Delete(fn,1,i);
    i:=length(fn);
    while (i>0) and (fn[i] in [',',' ',#9]) do
      dec(i);
    fn:=copy(fn,1,i);
  end;

  function GetName(var fn:string):string;
  var
    i : longint;
  begin
    i:=0;
    while (i<length(fn)) and (fn[i+1] in ['a'..'z','A'..'Z','0'..'9','_','-']) do
     inc(i);
    GetName:=Copy(fn,1,i);
    Delete(fn,1,i);
  end;

const
  maxlevel=16;
var
  f     : text;
  s,
  opts  : string;
  skip  : array[0..maxlevel-1] of boolean;
  level : longint;
  option_read : boolean;
begin
{ avoid infinite loop }
  Inc(FileLevel);
  Option_read:=false;
  If FileLevel>MaxLevel then
   Message(option_too_many_cfg_files);
{ open file }
  Message1(option_using_file,filename);
  assign(f,filename);
  {$I-}
  reset(f);
  {$I+}
  if ioresult<>0 then
   begin
     Message1(option_unable_open_file,filename);
     exit;
   end;
  fillchar(skip,sizeof(skip),0);
  level:=0;
  while not eof(f) do
   begin
     readln(f,opts);
     RemoveSep(opts);
     if (opts<>'') and (opts[1]<>';') then
      begin
        if opts[1]='#' then
         begin
           Delete(opts,1,1);
           s:=upper(GetName(opts));
           if (s='SECTION') then
            begin
              RemoveSep(opts);
              s:=upper(GetName(opts));
              if level=0 then
               skip[level]:=not (check_symbol(s) or (s='COMMON'));
            end
           else
            if (s='IFDEF') then
             begin
               RemoveSep(opts);
               if Level>=maxlevel then
                begin
                  Message(option_too_many_ifdef);
                  stopOptions;
                end;
               inc(Level);
               skip[level]:=(skip[level-1] or (not check_symbol(upper(GetName(opts)))));
             end
           else
            if (s='IFNDEF') then
             begin
               RemoveSep(opts);
               if Level>=maxlevel then
                begin
                  Message(option_too_many_ifdef);
                  stopOptions;
                end;
               inc(Level);
               skip[level]:=(skip[level-1] or (check_symbol(upper(GetName(opts)))));
             end
           else
            if (s='ELSE') then
             skip[level]:=skip[level-1] or (not skip[level])
           else
            if (s='ENDIF') then
             begin
               skip[level]:=false;
               if Level=0 then
                begin
                  Message(option_too_many_endif);
                  stopOptions;
                end;
               dec(level);
             end
           else
            if (not skip[level]) then
             begin
               if (s='DEFINE') then
                begin
                  RemoveSep(opts);
                  def_symbol(upper(GetName(opts)));
                end
              else
               if (s='UNDEF') then
                begin
                  RemoveSep(opts);
                  undef_symbol(upper(GetName(opts)));
                end
              else
               if (s='WRITE') then
                begin
                  Delete(opts,1,1);
                  WriteLn(opts);
                end
              else
               if (s='INCLUDE') then
                begin
                  Delete(opts,1,1);
                  Interpret_file(opts);
                end;
            end;
         end
        else
         begin
           if (opts[1]='-') or (opts[1]='@') then
            begin
              if (not skip[level]) then
                interpret_option(opts,false);
              Option_read:=true;
            end
           else
             Message1(option_illegal_para,opts);
         end;
      end;
   end;
  if Level>0 then
   Message(option_too_less_endif);
  if Not Option_read then
    Message1(option_no_option_found,filename);
  Close(f);
  Dec(FileLevel);
end;


procedure Toption.Interpret_envvar(const envname : string);
var
  argstart,
  env,
  pc     : pchar;
  arglen : longint;
  quote  : set of char;
  hs     : string;
begin
  Message1(option_using_env,envname);
  env:=GetEnvPChar(envname);
  pc:=env;
  if assigned(pc) then
   begin
     repeat
       { skip leading spaces }
       while pc^ in [' ',#9,#13] do
        inc(pc);
       case pc^ of
         #0 :
           break;
         '"' :
           begin
             quote:=['"'];
             inc(pc);
           end;
         '''' :
           begin
              quote:=[''''];
              inc(pc);
           end;
         else
           quote:=[' ',#9,#13];
       end;
     { scan until the end of the argument }
       argstart:=pc;
       while (pc^<>#0) and not(pc^ in quote) do
        inc(pc);
     { create argument }
       arglen:=pc-argstart;
       hs[0]:=chr(arglen);
       move(argstart^,hs[1],arglen);
       interpret_option(hs,true);
     { skip quote }
       if pc^ in quote then
        inc(pc);
     until false;
   end
  else
   Message1(option_no_option_found,'(env) '+envname);
  FreeEnvPChar(env);
end;


procedure toption.read_parameters;
var
  opts       : string;
  paramindex : longint;
begin
  paramindex:=0;
  while paramindex<paramcount do
   begin
     inc(paramindex);
     opts:=system.paramstr(paramindex);
     case opts[1] of
       '@' :
         if not firstpass then
         begin
           Delete(opts,1,1);
           Message1(option_reading_further_from,opts);
           interpret_file(opts);
         end;
       '!' :
         if not firstpass then
         begin
           Delete(opts,1,1);
           Message1(option_reading_further_from,'(env) '+opts);
           interpret_envvar(opts);
         end;
       else
         interpret_option(opts,true);
     end;
   end;
end;


procedure toption.parsecmd(cmd:string);
var
  i,ps  : longint;
  opts  : string;
begin
  while (cmd<>'') do
   begin
     while cmd[1]=' ' do
      delete(cmd,1,1);
     i:=pos(' ',cmd);
     if i=0 then
      i:=256;
     opts:=Copy(cmd,1,i-1);
     Delete(cmd,1,i);
     case opts[1] of
       '@' :
         if not firstpass then
         begin
           Delete(opts,1,1);
           Message1(option_reading_further_from,opts);
           interpret_file(opts);
         end;
       '!' :
         if not firstpass then
         begin
           Delete(opts,1,1);
           Message1(option_reading_further_from,'(env) '+opts);
           interpret_envvar(opts);
         end;
       '"' :
         begin
           Delete(opts,1,1);
           ps:=pos('"',cmd);
           if (i<>256) and (ps>0) then
             begin
               opts:=opts + ' '+ copy(cmd,1,ps-1);
               cmd:=copy(cmd,ps+1,255);
             end;
           interpret_option(opts,true);
         end;
       else
         interpret_option(opts,true);
     end;
   end;
end;


procedure toption.writequickinfo;
var
  s : string;
  i : longint;

  procedure addinfo(const hs:string);
  begin
    if s<>'' then
     s:=s+' '+hs
    else
     s:=hs;
  end;

begin
  s:='';
  i:=0;
  while (i<length(quickinfo)) do
   begin
     inc(i);
     case quickinfo[i] of
      'S' :
        begin
          inc(i);
          case quickinfo[i] of
           'O' :
             addinfo(lower(source_info.shortname));
{$ifdef Delphi}
           'P' :
             addinfo('i386');
{$else Delphi}
           'P' :
             addinfo(source_cpu_string);
{$endif Delphi}
           else
             IllegalPara('-iS'+QuickInfo);
          end;
        end;
      'T' :
        begin
          inc(i);
          case quickinfo[i] of
           'O' :
             addinfo(lower(target_info.shortname));
           'P' :
             AddInfo(target_cpu_string);
           else
             IllegalPara('-iT'+QuickInfo);
          end;
        end;
      'V' :
        AddInfo(version_string);
      'D' :
        AddInfo(date_string);
      '_' :
        ;
      else
        IllegalPara('-i'+QuickInfo);
    end;
  end;
  if s<>'' then
   begin
     writeln(s);
     stopoptions;
   end;
end;


constructor TOption.create;
begin
  DoWriteLogo:=false;
  NoPressEnter:=false;
  FirstPass:=false;
  FileLevel:=0;
  Quickinfo:='';
  ParaIncludePath:=TSearchPathList.Create;
  ParaObjectPath:=TSearchPathList.Create;
  ParaUnitPath:=TSearchPathList.Create;
  ParaLibraryPath:=TSearchPathList.Create;
end;


destructor TOption.destroy;
begin
  ParaIncludePath.Free;
  ParaObjectPath.Free;
  ParaUnitPath.Free;
  ParaLibraryPath.Free;
end;


{****************************************************************************
                              Callable Routines
****************************************************************************}

procedure read_arguments(cmd:string);
var
  configpath : pathstr;
begin
  option:=coption.create;
  disable_configfile:=false;

{ default defines }
  def_symbol(upper(target_info.shortname));
  def_symbol('FPC');
  def_symbol('VER'+version_nr);
  def_symbol('VER'+version_nr+'_'+release_nr);
  def_symbol('VER'+version_nr+'_'+release_nr+'_'+patch_nr);
{$ifdef newcg}
  def_symbol('WITHNEWCG');
{$endif}

{ Temporary defines, until things settle down }
{$ifdef SUPPORT_FIXED}
  def_symbol('HASFIXED');
{$endif SUPPORT_FIXED}
  def_symbol('HASWIDECHAR');
  def_symbol('HASOUT');
  def_symbol('HASINTF');
  def_symbol('HASVARIANT');
  def_symbol('INTERNSETLENGTH');
  def_symbol('INT64FUNCRESOK');
  def_symbol('PACKENUMFIXED');
  def_symbol('HAS_ADDR_STACK_ON_STACK');
  def_symbol('NOBOPUNDCHECK');

{ some stuff for TP compatibility }
{$ifdef i386}
  def_symbol('CPU86');
  def_symbol('CPU87');
{$endif}
{$ifdef m68k}
  def_symbol('CPU68');
{$endif}

{ new processor stuff }
{$ifdef i386}
  def_symbol('CPUI386');
{$endif}
{$ifdef m68k}
  def_symbol('CPU68K');
{$endif}
{$ifdef ALPHA}
  def_symbol('CPUALPHA');
{$endif}
{$ifdef powerpc}
  def_symbol('CPUPOWERPC');
{$endif}

{ get default messagefile }
{$ifdef Delphi}
  msgfilename:=dmisc.getenv('PPC_ERROR_FILE');
{$else Delphi}
  msgfilename:=dos.getenv('PPC_ERROR_FILE');
{$endif Delphi}

{ default configfile }
  if (cmd<>'') and (cmd[1]='[') then
   begin
     ppccfg:=Copy(cmd,2,pos(']',cmd)-2);
     Delete(cmd,1,pos(']',cmd));
   end
  else
   begin
{$ifdef i386}
     ppccfg:='ppc386.cfg';
{$endif i386}
{$ifdef ia64}
     ppccfg:='ppcia64.cfg';
{$endif ia64}
{$ifdef m68k}
     ppccfg:='ppc68k.cfg';
{$endif}
{$ifdef alpha}
     ppccfg:='ppcalpha.cfg';
{$endif}
{$ifdef powerpc}
     ppccfg:='ppcppc.cfg';
{$endif}
   end;

{ Order to read ppc386.cfg:
   1 - current dir
   2 - configpath
   3 - compiler path }
{$ifdef Delphi}
  configpath:=FixPath(dmisc.getenv('PPC_CONFIG_PATH'),false);
{$else Delphi}
  configpath:=FixPath(dos.getenv('PPC_CONFIG_PATH'),false);
{$endif Delphi}
{$ifdef Unix}
  if configpath='' then
   configpath:='/etc/';
{$endif}
  if ppccfg<>'' then
   begin
     read_configfile:=true;
     if not FileExists(ppccfg) then
      begin
{$ifdef Unix}
        if (dos.getenv('HOME')<>'') and FileExists(FixPath(dos.getenv('HOME'),false)+'.'+ppccfg) then
         ppccfg:=FixPath(dos.getenv('HOME'),false)+'.'+ppccfg
        else
{$endif}
         if FileExists(configpath+ppccfg) then
          ppccfg:=configpath+ppccfg
        else
{$ifndef Unix}
         if FileExists(exepath+ppccfg) then
          ppccfg:=exepath+ppccfg
        else
{$endif}
         read_configfile:=false;
      end;
   end
  else
   read_configfile:=false;

{ Read commandline and configfile }
  target_is_set:=false;
  asm_is_set:=false;

  param_file:='';

  if read_configfile then
   begin
   { read the parameters quick, only -i -v -T }
     option.firstpass:=true;
     if cmd<>'' then
       option.parsecmd(cmd)
     else
      begin
        option.read_parameters;
        { Write only quickinfo }
        if option.quickinfo<>'' then
         option.writequickinfo;
      end;
   { Read the configfile }
     option.firstpass:=false;
     if read_configfile then
      option.interpret_file(ppccfg);
   end;
  if cmd<>'' then
    option.parsecmd(cmd)
  else
    begin
      option.read_parameters;
      { Write only quickinfo }
      if option.quickinfo<>'' then
       option.writequickinfo;
    end;

{ Write help pages }
  if (cmd='') and (paramcount=0) then
   Option.WriteHelpPages;

{ Stop if errors in options }
  if ErrorCount>0 then
   StopOptions;

  { Non-core target defines }
  case target_info.target of
    target_i386_freebsd :
      begin
        def_symbol('LINUX'); { Hack: Linux define is also needed for freebsd (MvdV) }
        def_symbol('BSD');
        def_symbol('UNIX');
      end;
    target_i386_linux :
      begin
        def_symbol('UNIX');
      end;
    target_i386_sunos :
      begin
        def_symbol('UNIX');
        def_symbol('SOLARIS');
        def_symbol('LIBC');
        def_symbol('SUNOS');
      end;
  end;

{ write logo if set }
  if option.DoWriteLogo then
   option.WriteLogo;

{ Check file to compile }
  if param_file='' then
   begin
     Message(option_no_source_found);
     StopOptions;
   end;
{$ifndef Unix}
  param_file:=FixFileName(param_file);
{$endif}
  fsplit(param_file,inputdir,inputfile,inputextension);
  if inputextension='' then
   begin
     if FileExists(inputdir+inputfile+target_info.sourceext) then
      inputextension:=target_info.sourceext
     else
      if FileExists(inputdir+inputfile+target_info.pasext) then
       inputextension:=target_info.pasext;
   end;

{ Add paths specified with parameters to the searchpaths }
  UnitSearchPath.AddList(option.ParaUnitPath,true);
  ObjectSearchPath.AddList(option.ParaObjectPath,true);
  IncludeSearchPath.AddList(option.ParaIncludePath,true);
  LibrarySearchPath.AddList(option.ParaLibraryPath,true);

{ add unit environment and exepath to the unit search path }
  if inputdir<>'' then
   Unitsearchpath.AddPath(inputdir,true);
  if not disable_configfile then
{$ifdef Delphi}
  UnitSearchPath.AddPath(dmisc.getenv(target_info.unit_env),false);
{$else}
  UnitSearchPath.AddPath(dos.getenv(target_info.unit_env),false);
{$endif Delphi}
{$ifdef Unix}
  fpcdir:=FixPath(getenv('FPCDIR'),false);
  if fpcdir='' then
   begin
     if PathExists('/usr/local/lib/fpc/'+version_string) then
      fpcdir:='/usr/local/lib/fpc/'+version_string+'/'
     else
      fpcdir:='/usr/lib/fpc/'+version_string+'/';
   end;
{$else}
  fpcdir:=FixPath(getenv('FPCDIR'),false);
  if fpcdir='' then
   begin
     fpcdir:=ExePath+'../';
     if not(PathExists(fpcdir+'/units')) and
        not(PathExists(fpcdir+'/rtl')) then
      fpcdir:=fpcdir+'../';
   end;
{$endif}
  { first try development RTL, else use the default installation path }
  if not disable_configfile then
    begin
      if PathExists(FpcDir+'rtl/'+lower(target_info.shortname)) then
       UnitSearchPath.AddPath(FpcDir+'rtl/'+lower(target_info.shortname),false)
      else
       begin
         UnitSearchPath.AddPath(FpcDir+'units/'+lower(target_info.shortname),false);
         UnitSearchPath.AddPath(FpcDir+'units/'+lower(target_info.shortname)+'/rtl',false);
       end;
    end;
  { Add exepath if the exe is not in the current dir, because that is always searched already }
  if ExePath<>GetCurrentDir then
   UnitSearchPath.AddPath(ExePath,false);
  { Add unit dir to the object and library path }
  objectsearchpath.AddList(unitsearchpath,false);
  librarysearchpath.AddList(unitsearchpath,false);

{ switch assembler if it's binary and we got -a on the cmdline }
  if (cs_asm_leave in initglobalswitches) and
     (target_asm.outputbinary) then
   begin
     Message(option_switch_bin_to_src_assembler);
     set_target_asm(target_info.assemextern);
   end;

  if (target_asm.supported_target <> target_any) and
     (target_asm.supported_target <> target_info.target) then
   begin
     Message2(option_incompatible_asm,target_asm.idtxt,target_info.name);
     set_target_asm(target_info.assemextern);
     Message1(option_asm_forced,target_asm.idtxt);
   end;

{ turn off stripping if compiling with debuginfo or profile }
  if (cs_debuginfo in initmoduleswitches) or
     (cs_profile in initmoduleswitches) then
    initglobalswitches:=initglobalswitches-[cs_link_strip];

  if not LinkTypeSetExplicitly then
   set_default_link_type;

{ Set defines depending on the target }
  if (target_info.target in [target_i386_GO32V1,target_i386_GO32V2]) then
   def_symbol('DPMI'); { MSDOS is not defined in BP when target is DPMI }

  option.free;
  Option:=nil;
end;


initialization
  coption:=toption;
finalization
  if assigned(option) then
   option.free;
end.
{
  $Log$
  Revision 1.41  2001-05-12 12:11:31  peter
    * simplify_ppu is now the default, a recompile of the compiler now
      only compiles pp.pas

  Revision 1.40  2001/04/18 22:01:54  peter
    * registration of targets and assemblers

  Revision 1.39  2001/04/13 01:22:10  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.38  2001/03/25 12:27:31  peter
    * fixed -Se (merged)

  Revision 1.37  2001/03/23 00:16:07  florian
    + some stuff to compile FreeCLX added

  Revision 1.36  2001/03/13 20:59:56  peter
    * message loading fixes from Sergey (merged)

  Revision 1.35  2001/03/10 13:19:10  peter
    * don't check messagefile for numbers, this allows the usage of
      1.1 msgfiles with a 1.0.x compiler

  Revision 1.34  2001/03/05 21:50:29  peter
    * press enter moved to errore.msg

  Revision 1.33  2001/03/03 12:41:22  jonas
    * simplified and optimized range checking code, FPC_BOUNDCHECK is no longer necessary

  Revision 1.32  2001/02/26 19:44:53  peter
    * merged generic m68k updates from fixes branch

  Revision 1.31  2001/02/26 12:47:46  jonas
    * fixed bug in type checking for compatibility of set elements (merged)
    * released fix in options.pas from Carl also for FPC (merged)

  Revision 1.30  2001/02/26 08:08:39  michael
  * option_help_pages:
     allow to omit an option (use one space char insteed an option)
     but to indent a continuation line as if option is present. For lines:
       3*2CX_first line
       3*2 _second line
       3*2*_third line
     we could get:
       -CX        first line
                  second line
       third line

  Revision 1.29  2001/02/26 07:49:50  michael
  Support replacements for all -F<x> options

  Revision 1.28  2001/02/05 21:26:36  peter
    * applied patches from Sergey Korshunoff

  Revision 1.27  2001/01/20 18:36:51  hajny
    + APPTYPE support under OS/2, app_fs, GetEnvPChar for OS/2

  Revision 1.26  2001/01/12 19:21:09  peter
    * fixed writing of quickinfo when no ppc386.cfg is available

  Revision 1.25  2001/01/05 17:36:57  florian
  * the info about exception frames is stored now on the stack
  instead on the heap

  Revision 1.24  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.23  2000/12/24 12:21:41  peter
    * use system.paramstr()

  Revision 1.22  2000/12/23 19:46:49  peter
    * object to class conversion
    * more verbosity for -vt and -vd
    * -i options can be put after eachother so the Makefiles only need
      to call fpc once for all info (will be twice as the first one will
      be to check the version if fpc supports multiple info)

  Revision 1.21  2000/12/16 15:56:19  jonas
    - removed all ifdef cardinalmulfix code

  Revision 1.20  2000/12/15 13:26:01  jonas
    * only return int64's from functions if it int64funcresok is defined
    + added int64funcresok define to options.pas

  Revision 1.19  2000/11/30 22:48:23  florian
  * opts386 renamed

  Revision 1.18  2000/11/29 00:30:34  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.17  2000/11/13 15:26:12  marco
   * Renamefest

  Revision 1.16  2000/11/12 22:20:37  peter
    * create generic toutputsection for binary writers

  Revision 1.15  2000/11/07 15:09:27  marco
   * Define UNIX for FreeBSD and Linux. Checked crosscompile thingy.

  Revision 1.14  2000/11/07 14:25:08  marco
   * FreeBSD defines (FreeBSD,Linux,BSD,Unix) Linux defines (Linux,Unix)

  Revision 1.13  2000/11/06 20:30:54  peter
    * more fixes to get make cycle working

  Revision 1.12  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.11  2000/09/26 10:50:41  jonas
    * initmodeswitches is changed is you change the compiler mode from the
      command line (the -S<x> switches didn't work anymore for changing the
      compiler mode) (merged from fixes branch)

  Revision 1.10  2000/09/24 21:33:47  peter
    * message updates merges

  Revision 1.9  2000/09/24 15:06:20  peter
    * use defines.inc

  Revision 1.8  2000/09/18 12:28:41  marco
   * Definition of multiple FreeBSD target defines moved to after error check
      commandline parsing

  Revision 1.7  2000/09/16 12:22:52  peter
    * freebsd support merged

  Revision 1.6  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.5  2000/08/07 11:31:04  jonas
    * fixed bug in type conversions between enum subranges (it didn't take
      the packenum directive into account)
    + define PACKENUMFIXED symbol in options.pas
     (merged from fixes branch)

  Revision 1.4  2000/07/14 05:11:48  michael
  + Patch to 1.1

  Revision 1.3  2000/07/13 12:08:26  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:44  michael
  + removed logs

}
