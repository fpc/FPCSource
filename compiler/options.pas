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

var
  coption : class of toption;

procedure read_arguments(cmd:string);


implementation

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  version,systems,
  cutils,cobjects,messages
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
  msgfilename,
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


procedure MaybeLoadMessageFile;
begin
{ Load new message file }
  if (msgfilename<>'')  then
    begin
       if fileexists(msgfilename) then
         LoadMsgFile(msgfilename);
       msgfilename:='';
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
  MaybeLoadMessageFile;
  p:=MessagePchar(option_logo);
  while assigned(p) do
   Comment(V_Normal,GetMsgLine(p));
end;


procedure Toption.WriteInfo;
var
  p : pchar;
begin
  MaybeLoadMessageFile;
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
  MaybeLoadMessageFile;
  Message1(option_usage,system.paramstr(0));
  lastident:=0;
  if DoWriteLogo then
   lines:=3
  else
   lines:=1;
  p:=MessagePChar(option_help_pages);
  while assigned(p) do
   begin
   { get a line and reset }
     s:=GetMsgLine(p);
     ident:=0;
     show:=false;
   { parse options }
     case s[1] of
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
                 ident:=11;
                 outline:=11;
               end;
         '3' : begin
                 ident:=21;
                 outline:=6;
               end;
        end;
        j:=pos('_',s);
        opt:=Copy(s,4,j-4);
        if opt='*' then
         opt:=''
        else
         opt:=PadEnd('-'+opt,outline);
        if (ident=0) and (lastident<>0) then
         begin
           Comment(V_Normal,'');
           inc(Lines);
         end;
      { page full ? }
        if (lines>=page_size) then
         begin
           if not NoPressEnter then
            begin
              write('*** press enter ***');
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
                        '-' : initglobalswitches:=initglobalswitches-[cs_asm_leave,cs_asm_source,cs_asm_regalloc];
                       else
                         IllegalPara(opt);
                       end;
                    end;
              'A' : begin
                      if set_string_asm(More) then
                       begin
                         initoutputformat:=target_asm.id;
                         asm_is_set:=true;
                       end
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
              'B' : if more='' then
                      do_build:=true
                    else
                      if more = '-' then
                        do_build := False
                      else
                        IllegalPara(opt);
              'C' : begin
                      j := 1;
                      while j <= length(more) Do
                        Begin
                          case more[j] of
                            'a' : Simplify_ppu:=true;
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
                      case c of
                       'D' : begin
                              if not ispara then
                               DefaultReplacements(More);
                              utilsdirectory:=FixPath(More,true);
                             end;
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
              'l' : if more='' then
                      DoWriteLogo:=true
                    else
                      IllegalPara(opt);
              'm' : parapreprocess:=true;
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
              'P' : initglobalswitches:=initglobalswitches+[cs_asm_pipe];
{$endif}
              's' : initglobalswitches:=initglobalswitches+[cs_asm_extern,cs_link_extern];
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
                                  SetErrorFlags(more);
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
                          'v' : Message1(option_obsolete_switch,'-Sv');
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
                         undef_symbol(target_info.short_name);
                       { load new target }
                         if not(set_string_target(More)) then
                           IllegalPara(opt);
                       { set new define }
                         def_symbol(target_info.short_name);
                         if not asm_is_set then
                           initoutputformat:=target_asm.id;
                         target_is_set:=true;
                       end
                      else
                       if More<>target_info.short_name then
                        Message1(option_target_is_already_set,target_info.short_name);
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
                               {  -WB200000 means set prefered base address
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
                        'D': ForceDeffileForExport:=true;
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
           if (opts[1]='-') then
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
         begin
           Delete(opts,1,1);
           if not firstpass then
            Message1(option_reading_further_from,opts);
           interpret_file(opts);
         end;
       '!' :
         begin
           Delete(opts,1,1);
           if not firstpass then
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
         begin
           Delete(opts,1,1);
           if not firstpass then
            Message1(option_reading_further_from,opts);
           interpret_file(opts);
         end;
       '!' :
         begin
           Delete(opts,1,1);
           if not firstpass then
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
             addinfo(source_os.shortname);
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
             addinfo(target_os.shortname);
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

{ Load messages }
  if (cmd='') and (paramcount=0) then
   option.WriteHelpPages;

  disable_configfile:=false;

{ default defines }
  def_symbol(target_info.short_name);
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
  def_symbol('INTERNSETLENGTH');
  def_symbol('INT64FUNCRESOK');
  def_symbol('PACKENUMFIXED');

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
       option.read_parameters;
     option.firstpass:=false;
   { Write only quickinfo }
     if option.quickinfo<>'' then
      option.writequickinfo;
   { Read the configfile }
     if read_configfile then
      option.interpret_file(ppccfg);
   end;
  if cmd<>'' then
    option.parsecmd(cmd)
  else
    option.read_parameters;

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
     if FileExists(inputdir+inputfile+target_os.sourceext) then
      inputextension:=target_os.sourceext
     else
      if FileExists(inputdir+inputfile+target_os.pasext) then
       inputextension:=target_os.pasext;
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
      if PathExists(FpcDir+'rtl/'+lower(target_info.short_name)) then
       UnitSearchPath.AddPath(FpcDir+'rtl/'+lower(target_info.short_name),false)
      else
       begin
         UnitSearchPath.AddPath(FpcDir+'units/'+lower(target_info.short_name),false);
         UnitSearchPath.AddPath(FpcDir+'units/'+lower(target_info.short_name)+'/rtl',false);
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
     (target_asm.id in binassem) then
   begin
     Message(option_switch_bin_to_src_assembler);
     set_target_asm(target_info.assemsrc);
     initoutputformat:=target_asm.id;
   end;

  if (target_asm.supported_target <> target_any) and
     (target_asm.supported_target <> target_info.target) then
   begin
     Message2(option_incompatible_asm,target_asm.idtxt,target_os.name);
     { Should we reset to default ??? }
     set_target_asm(target_info.assemsrc);
     Message1(option_asm_forced,target_asm.idtxt);
     initoutputformat:=target_asm.id;
   end;

{ turn off stripping if compiling with debuginfo or profile }
  if (cs_debuginfo in initmoduleswitches) or
     (cs_profile in initmoduleswitches) then
    initglobalswitches:=initglobalswitches-[cs_link_strip];

  if not LinkTypeSetExplicitly then
    begin
      if (target_os.id=os_i386_win32) then
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
{ Set defines depending on the target }
  if (target_info.target in [target_i386_GO32V1,target_i386_GO32V2]) then
   def_symbol('DPMI'); { MSDOS is not defined in BP when target is DPMI }

  MaybeLoadMessageFile;

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
  Revision 1.24  2000-12-25 00:07:26  peter
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
