{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

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

{$i fpcdefs.inc}

interface

uses
  globtype,globals,verbose,systems,cpuinfo;

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
    ParaAlignment   : TAlignmentInfo;
    Constructor Create;
    Destructor Destroy;override;
    procedure WriteLogo;
    procedure WriteInfo;
    procedure WriteHelpPages;
    procedure WriteQuickInfo;
    procedure IllegalPara(const opt:string);
    function  Unsetbool(var Opts:string; Pos: Longint):boolean;
    procedure interpret_proc_specific_options(const opt:string);virtual;
    procedure interpret_option(const opt :string;ispara:boolean);
    procedure Interpret_envvar(const envname : string);
    procedure Interpret_file(const filename : string);
    procedure Read_Parameters;
    procedure parsecmd(cmd:string);
    procedure TargetDefines(def:boolean);
  end;

  TOptionClass=class of toption;

var
  coption : TOptionClass;

procedure read_arguments(cmd:string);


implementation

uses
  widestr,
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  version,
  cutils,cmsgs
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
  ppcaltcfg,
  param_file    : string;   { file to compile specified on the commandline }

{****************************************************************************
                                 Defines
****************************************************************************}

procedure def_symbol(const s : string);
begin
  if s='' then
   exit;
  initdefines.insert(upper(s));
  Message1(option_defining_symbol,s);
end;


procedure undef_symbol(const s : string);
begin
  if s='' then
   exit;
  InitDefines.Remove(s);
  Message1(option_undefining_symbol,s);
end;


function check_symbol(const s:string):boolean;
begin
  check_symbol:=(initdefines.find(s)<>nil);
end;


procedure set_default_link_type;
begin
  if (target_info.system in [system_i386_win32,system_i386_wdosx]) then
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
  hs,hs1,s : string;
  target : tsystem;
begin
  p:=MessagePchar(option_info);
  while assigned(p) do
   begin
     s:=GetMsgLine(p);
     { list OS Targets }
     if pos('$OSTARGETS',s)>0 then
      begin
        for target:=low(tsystem) to high(tsystem) do
         if assigned(targetinfos[target]) then
          begin
            hs:=s;
            hs1:=targetinfos[target]^.name;
            if tf_under_development in targetinfos[target]^.flags then
             hs1:=hs1+' (under development)';
            Replace(hs,'$OSTARGETS',hs1);
            Comment(V_Normal,hs);
          end;
      end
     else
      Comment(V_Normal,s);
   end;
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
{$ifdef powerpc}
      'P',
{$endif}
{$ifdef vis}
      'V',
{$endif}
{$ifdef sparc}
      'S',
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


function Toption.Unsetbool(var Opts:string; Pos: Longint):boolean;
{ checks if the character after pos in Opts is a + or a - and returns resp.
  false or true. If it is another character (or none), it also returns false }
begin
  UnsetBool := false;
  if Length(Opts)>Pos then
   begin
    inc(Pos);
    UnsetBool := Opts[Pos] = '-';
    if Opts[Pos] in ['-','+']then
     delete(Opts,Pos,1);
   end;
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
     not((opt[1]='-') and (opt[2] in ['i','d','v','V','T','u','n','X'])) then
   exit;

  Message1(option_handling_option,opt);
  case opt[1] of
    '-' :
      begin
         more:=Copy(opt,3,255);
         if firstpass then
           Message1(option_interpreting_firstpass_option,opt)
         else
           Message1(option_interpreting_option,opt);
         case opt[2] of
           '?' :
             WriteHelpPages;

           'a' :
             begin
               include(initglobalswitches,cs_asm_leave);
               j:=1;
               while j<=length(more) do
                begin
                  case more[j] of
                    'l' :
                      include(initglobalswitches,cs_asm_source);
                    'r' :
                      include(initglobalswitches,cs_asm_regalloc);
                    't' :
                      include(initglobalswitches,cs_asm_tempalloc);
                    'n' :
                      include(initglobalswitches,cs_asm_nodes);
                    '-' :
                      initglobalswitches:=initglobalswitches -
                          [cs_asm_leave, cs_asm_source,cs_asm_regalloc, cs_asm_tempalloc, cs_asm_nodes];
                    else
                      IllegalPara(opt);
                  end;
                  inc(j);
                end;
             end;

           'A' :
             begin
               if set_target_asm_by_string(More) then
                asm_is_set:=true
               else
                IllegalPara(opt);
             end;

           'b' :
             begin
               if UnsetBool(More,0) then
                begin
                  exclude(initmoduleswitches,cs_browser);
                  exclude(initmoduleswitches,cs_local_browser);
{$ifdef BrowserLog}
                  exclude(initglobalswitches,cs_browser_log);
{$endif}
                end
               else
                begin
                  include(initmoduleswitches,cs_browser);
{$ifdef BrowserLog}
                  include(initglobalswitches,cs_browser_log);
{$endif}
                end;
               if More<>'' then
                 if (More='l') or (More='l+') then
                   include(initmoduleswitches,cs_local_browser)
                 else
                  if More='l-' then
                   exclude(initmoduleswitches,cs_local_browser)
                 else
{$ifdef BrowserLog}
                   browserlog.elements_to_list.insert(more);
{$else}
                   IllegalPara(opt);
{$endif}
             end;

           'B' :
             do_build:=not UnSetBool(more,0);

           'C' :
             begin
               j:=1;
               while j<=length(more) do
                begin
                  case more[j] of
                    'a' :
                      Message2(option_obsolete_switch_use_new,'-Ca','-Or');
                    'c' :
                       begin
                         if not SetAktProcCall(upper(copy(more,j+1,length(more)-j)),true) then
                          IllegalPara(opt);
                         break;
                       end;
                    'e' :
                       begin
                         If UnsetBool(More, j) then
                           exclude(initmoduleswitches,cs_fp_emulation)
                         Else
                           include(initmoduleswitches,cs_fp_emulation);
                       end; 
                    'h' :
                       begin
                         val(copy(more,j+1,length(more)-j),heapsize,code);
                         if (code<>0) or (heapsize>=67107840) or (heapsize<1024) then
                          IllegalPara(opt);
                         break;
                       end;
                    'i' :
                      If UnsetBool(More, j) then
                        exclude(initlocalswitches,cs_check_io)
                      else
                        include(initlocalswitches,cs_check_io);
                    'n' :
                      If UnsetBool(More, j) then
                        exclude(initglobalswitches,cs_link_extern)
                      Else
                        include(initglobalswitches,cs_link_extern);
                    'o' :
                      If UnsetBool(More, j) then
                        exclude(initlocalswitches,cs_check_overflow)
                      Else
                        include(initlocalswitches,cs_check_overflow);
                    'r' :
                      If UnsetBool(More, j) then
                        exclude(initlocalswitches,cs_check_range)
                      Else
                        include(initlocalswitches,cs_check_range);
                    'R' :
                      If UnsetBool(More, j) then
                        begin
                          exclude(initlocalswitches,cs_check_range);
                          exclude(initlocalswitches,cs_check_object);
                        end
                      Else
                        begin
                          include(initlocalswitches,cs_check_range);
                          include(initlocalswitches,cs_check_object);
                        end;
                    's' :
                      begin
                         val(copy(more,j+1,length(more)-j),stacksize,code);
                         if (code<>0) or (stacksize>=67107840) or (stacksize<1024) then
                          IllegalPara(opt);
                         break;
                      end;
                    't' :
                       If UnsetBool(More, j) then
                         exclude(initlocalswitches,cs_check_stack)
                       Else
                         include(initlocalswitches,cs_check_stack);
                    'D' :
                       If UnsetBool(More, j) then
                         exclude(initmoduleswitches,cs_create_dynamic)
                       Else
                         include(initmoduleswitches,cs_create_dynamic);
                    'X' :
                       If UnsetBool(More, j) then
                         exclude(initmoduleswitches,cs_create_smart)
                       Else
                         include(initmoduleswitches,cs_create_smart);
                    else
                       IllegalPara(opt);
                  end;
                  inc(j);
                end;
             end;

           'd' :
             def_symbol(more);

           'D' :
             begin
               include(initglobalswitches,cs_link_deffile);
               j:=1;
               while j<=length(more) do
                begin
                  case more[j] of
                    'd' :
                      begin
                        description:=Copy(more,j+1,255);
                        break;
                      end;
                    'v' :
                      begin
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
                           else
                             if error=0 then
                               error:=1;
                         end;
                        if l=0 then
                          l:=256;
                        dllmajor:=1;
                        if error=0 then
                          valint(copy(dllversion,1,l-1),major,error);
                        if (error=0) and (major>=0) and (major<=$ffff) then
                          dllmajor:=major
                        else
                          if error=0 then
                            error:=1;
                        if error<>0 then
                          Message1(scan_w_wrong_version_ignored,dllversion);
                        break;
                      end;
                    'w' :
                      usewindowapi:=true;
                    '-' :
                      begin
                        exclude(initglobalswitches,cs_link_deffile);
                        usewindowapi:=false;
                      end;
                    else
                      IllegalPara(opt);
                  end;
                  inc(j);
                end;
             end;

           'e' :
             exepath:=FixPath(More,true);

           'E' :
             begin
               if UnsetBool(More, 0) then
                 exclude(initglobalswitches,cs_link_extern)
               else
                 include(initglobalswitches,cs_link_extern);
             end;

           'F' :
             begin
               c:=more[1];
               Delete(more,1,1);
               DefaultReplacements(More);
               case c of
                 'c' :
                   begin
                     if not(cpavailable(more)) then
                       Message1(option_code_page_not_available,more)
                     else
                       initsourcecodepage:=more;
                   end;
                 'D' :
                   utilsdirectory:=FixPath(More,true);
                 'e' :
                   SetRedirectFile(More);
                 'E' :
                   OutputExeDir:=FixPath(More,true);
                 'i' :
                   begin
                     if ispara then
                       ParaIncludePath.AddPath(More,false)
                     else
                       includesearchpath.AddPath(More,true);
                   end;
                 'g' :
                   Message2(option_obsolete_switch_use_new,'-Fg','-Fl');
                 'l' :
                   begin
                     if ispara then
                       ParaLibraryPath.AddPath(More,false)
                     else
                       LibrarySearchPath.AddPath(More,true);
                   end;
                 'L' :
                   begin
                     if More<>'' then
                       ParaDynamicLinker:=More
                     else
                       IllegalPara(opt);
                   end;
                 'o' :
                   begin
                     if ispara then
                       ParaObjectPath.AddPath(More,false)
                     else
                       ObjectSearchPath.AddPath(More,true);
                   end;
                 'r' :
                   Msgfilename:=More;
                 'u' :
                   begin
                     if ispara then
                       ParaUnitPath.AddPath(More,false)
                     else
                       unitsearchpath.AddPath(More,true);
                   end;
                 'U' :
                   OutputUnitDir:=FixPath(More,true);
                 else
                   IllegalPara(opt);
               end;
             end;
       'g' : begin
               if UnsetBool(More, 0) then
                begin
                  exclude(initmoduleswitches,cs_debuginfo);
                  exclude(initglobalswitches,cs_gdb_dbx);
                  exclude(initglobalswitches,cs_gdb_gsym);
                  exclude(initglobalswitches,cs_gdb_heaptrc);
                  exclude(initglobalswitches,cs_gdb_lineinfo);
                  exclude(initglobalswitches,cs_checkpointer);
                end
               else
                begin
{$ifdef GDB}
                  include(initmoduleswitches,cs_debuginfo);
                  if not RelocSectionSetExplicitly then
                    RelocSection:=false;
                  j:=1;
                  while j<=length(more) do
                    begin
                      case more[j] of
                        'd' :
                          begin
                            if UnsetBool(More, j) then
                              exclude(initglobalswitches,cs_gdb_dbx)
                            else
                              include(initglobalswitches,cs_gdb_dbx);
                          end;
                       'g' :
                          begin
                            if UnsetBool(More, j) then
                              exclude(initglobalswitches,cs_gdb_gsym)
                            else
                              include(initglobalswitches,cs_gdb_gsym);
                          end;
                        'h' :
                          begin
                            if UnsetBool(More, j) then
                              exclude(initglobalswitches,cs_gdb_heaptrc)
                            else
                              include(initglobalswitches,cs_gdb_heaptrc);
                          end;
                        'l' :
                          begin
                            if UnsetBool(More, j) then
                              exclude(initglobalswitches,cs_gdb_lineinfo)
                            else
                              include(initglobalswitches,cs_gdb_lineinfo);
                          end;
                        'c' :
                          begin
                            if UnsetBool(More, j) then
                              exclude(initglobalswitches,cs_checkpointer)
                            else
                              include(initglobalswitches,cs_checkpointer);
                          end;
                        else
                          IllegalPara(opt);
                      end;
                      inc(j);
                    end;
{$else GDB}
                  Message(option_no_debug_support);
                  Message(option_no_debug_support_recompile_fpc);
{$endif GDB}
                end;
             end;

           'h' :
             begin
               NoPressEnter:=true;
               WriteHelpPages;
             end;

           'i' :
             begin
               if More='' then
                 WriteInfo
               else
                 QuickInfo:=QuickInfo+More;
             end;

           'I' :
             begin
               if ispara then
                 ParaIncludePath.AddPath(More,false)
               else
                includesearchpath.AddPath(More,false);
             end;

           'k' :
             begin
               if more<>'' then
                 ParaLinkOptions:=ParaLinkOptions+' '+More
               else
                 IllegalPara(opt);
             end;

           'l' :
             DoWriteLogo:=not UnSetBool(more,0);

           'm' :
             parapreprocess:=not UnSetBool(more,0);

           'n' :
             begin
               if More='' then
                 disable_configfile:=true
               else
                 IllegalPara(opt);
             end;

           'o' :
             begin
               if More<>'' then
                 Fsplit(More,d,OutputFile,e)
               else
                 IllegalPara(opt);
             end;

           'p' :
             begin
               if UnsetBool(More, 0) then
                 begin
                   initmoduleswitches:=initmoduleswitches-[cs_profile];
                   undef_symbol('FPC_PROFILE');
                 end
               else
                 if Length(More)=0 then
                   IllegalPara(opt)
                 else
                 case more[1] of
                  'g' : if UnsetBool(more, 1) then
                         begin
                           exclude(initmoduleswitches,cs_profile);
                           undef_symbol('FPC_PROFILE');
                         end
                        else
                         begin
                           include(initmoduleswitches,cs_profile);
                           def_symbol('FPC_PROFILE');
                        end;
                 else
                   IllegalPara(opt);
                 end;
             end;

{$ifdef Unix}
           'P' :
             begin
               if UnsetBool(More, 0) then
                 exclude(initglobalswitches,cs_asm_pipe)
               else
                 include(initglobalswitches,cs_asm_pipe);
             end;
{$endif Unix}

           's' :
             begin
               if UnsetBool(More, 0) then
                 begin
                   initglobalswitches:=initglobalswitches-[cs_asm_extern,cs_link_extern];
                   if more<>'' then
                     IllegalPara(opt);
                 end
               else
                 begin
                   initglobalswitches:=initglobalswitches+[cs_asm_extern,cs_link_extern];
                   if more='h' then
                     initglobalswitches:=initglobalswitches-[cs_link_on_target]
                   else if more='t' then
                     initglobalswitches:=initglobalswitches+[cs_link_on_target]
                   else if more<>'' then
                     IllegalPara(opt);
                 end;
             end;

           'S' :
             begin
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
                begin
                  j:=1;
                  while j<=length(more) do
                   begin
                     case more[j] of
                       '2' :
                         SetCompileMode('OBJFPC',true);
                       'a' :
                         include(initlocalswitches,cs_do_assertion);
                       'c' :
                         include(initmoduleswitches,cs_support_c_operators);
                       'd' :
                         SetCompileMode('DELPHI',true);
                       'e' :
                         begin
                           SetErrorFlags(copy(more,j+1,length(more)));
                           break;
                         end;
                       'g' :
                         include(initmoduleswitches,cs_support_goto);
                       'h' :
                         include(initlocalswitches,cs_ansistrings);
                       'i' :
                         include(initmoduleswitches,cs_support_inline);
                       'm' :
                         include(initmoduleswitches,cs_support_macro);
                       'o' :
                         SetCompileMode('TP',true);
                       'p' :
                         SetCompileMode('GPC',true);
                       's' :
                         include(initglobalswitches,cs_constructor_name);
                       't' :
                         include(initmoduleswitches,cs_static_keyword);
                       '-' :
                         begin
                           exclude(initglobalswitches,cs_constructor_name);
                           initlocalswitches:=InitLocalswitches - [cs_do_assertion, cs_ansistrings];
                           initmoduleswitches:=initmoduleswitches - [cs_support_c_operators, cs_support_goto,
                                                                     cs_support_inline, cs_support_macro,
                                                                     cs_static_keyword];
                         end;
                       else
                         IllegalPara(opt);
                     end;
                     inc(j);
                   end;
                end;
             end;

           'T' :
             begin
               more:=Upper(More);
               if not target_is_set then
                begin
                  { remove old target define }
                  TargetDefines(false);
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
                  TargetDefines(true);
                  target_is_set:=true;
                end
               else
                if More<>upper(target_info.shortname) then
                 Message1(option_target_is_already_set,target_info.shortname);
             end;

           'u' :
             undef_symbol(upper(More));

           'U' :
             begin
               j:=1;
               while j<=length(more) do
                begin
                  case more[j] of
{$ifdef UNITALIASES}
                    'a' :
                       begin
                         AddUnitAlias(Copy(More,j+1,255));
                         break;
                       end;
{$endif UNITALIASES}
                    'n' :
                      exclude(initglobalswitches,cs_check_unit_name);
                    'p' :
                       begin
                         Message2(option_obsolete_switch_use_new,'-Up','-Fu');
                         break;
                       end;
                    'r' :
                      do_release:=true;
                    's' :
                      include(initmoduleswitches,cs_compilesystem);
                    '-' :
                      begin
                        exclude(initmoduleswitches,cs_compilesystem);
                        exclude(initglobalswitches,cs_check_unit_name);
                      end;
                    else
                      IllegalPara(opt);
                  end;
                  inc(j);
                end;
             end;

           'v' :
             begin
               if not setverbosity(More) then
                 IllegalPara(opt);
             end;

           'V' :
             PrepareReport;

           'W' :
             begin
               j:=1;
               while j<=length(More) do
                begin
                  case More[j] of
                    'B':
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
                    'C':
                      begin
                        if UnsetBool(More, j) then
                          apptype:=app_gui
                        else
                          apptype:=app_cui;
                      end;
                    'D':
                      ForceDeffileForExport:=not UnsetBool(More, j);
                    'F':
                      apptype:=app_fs;
                    'G':
                      begin
                        if UnsetBool(More, j) then
                          apptype:=app_cui
                        else
                          apptype:=app_gui;
                      end;
                    'N':
                      begin
                        RelocSection:=UnsetBool(More,j);
                        RelocSectionSetExplicitly:=true;
                      end;
                    'R':
                      begin
                        { support -WR+ / -WR- as synonims to -WR / -WN }
                        RelocSection:=not UnsetBool(More,j);
                        RelocSectionSetExplicitly:=true;
                      end;
                    else
                      IllegalPara(opt);
                  end;
                  inc(j);
                end;
             end;

           'X' :
             begin
               j:=1;
               while j<=length(more) do
                begin
                  case More[j] of
                    'i' :
                      include(initglobalswitches,cs_link_internal);
                    'm' :
                      include(initglobalswitches,cs_link_map);
                    's' :
                      include(initglobalswitches,cs_link_strip);
                    't' :
                      include(initglobalswitches,cs_link_staticflag);
                    'D' :
                      begin
                        def_symbol('FPC_LINK_DYNAMIC');
                        undef_symbol('FPC_LINK_SMART');
                        undef_symbol('FPC_LINK_STATIC');
                        exclude(initglobalswitches,cs_link_static);
                        exclude(initglobalswitches,cs_link_smart);
                        include(initglobalswitches,cs_link_shared);
                        LinkTypeSetExplicitly:=true;
                      end;
                    'S' :
                      begin
                        def_symbol('FPC_LINK_STATIC');
                        undef_symbol('FPC_LINK_SMART');
                        undef_symbol('FPC_LINK_DYNAMIC');
                        include(initglobalswitches,cs_link_static);
                        exclude(initglobalswitches,cs_link_smart);
                        exclude(initglobalswitches,cs_link_shared);
                        LinkTypeSetExplicitly:=true;
                      end;
                    'X' :
                      begin
                        def_symbol('FPC_LINK_SMART');
                        undef_symbol('FPC_LINK_STATIC');
                        undef_symbol('FPC_LINK_DYNAMIC');
                        exclude(initglobalswitches,cs_link_static);
                        include(initglobalswitches,cs_link_smart);
                        exclude(initglobalswitches,cs_link_shared);
                        LinkTypeSetExplicitly:=true;
                      end;
                    '-' :
                      begin
                        exclude(initglobalswitches,cs_link_staticflag);
                        exclude(initglobalswitches,cs_link_strip);
                        exclude(initglobalswitches,cs_link_map);
                        set_default_link_type;
                      end;
                    else
                      IllegalPara(opt);
                  end;
                  inc(j);
                end;
             end;

           { give processor specific options a chance }
           else
             interpret_proc_specific_options(opt);
         end;
       end;

    '@' :
      begin
        Message(option_no_nested_response_file);
        StopOptions;
      end;

    else
      begin
        if (length(param_file)<>0) then
          Message(option_only_one_source_support);
        param_file:=opt;
        Message1(option_found_file,opt);
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
  Message1(option_start_reading_configfile,filename);
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
           Message1(option_interpreting_file_option,opts);
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
    Message1(option_no_option_found,filename)
  else
    Message1(option_end_reading_configfile,filename);
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


procedure TOption.TargetDefines(def:boolean);
var
  s : string;
  i : integer;
begin
  if def then
   def_symbol(upper(target_info.shortname))
  else
   undef_symbol(upper(target_info.shortname));
  s:=target_info.extradefines;
  while (s<>'') do
   begin
     i:=pos(';',s);
     if i=0 then
      i:=length(s)+1;
     if def then
      def_symbol(Copy(s,1,i-1))
     else
      undef_symbol(Copy(s,1,i-1));
     delete(s,1,i);
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
  FillChar(ParaAlignment,sizeof(ParaAlignment),0);
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

function check_configfile(const fn:string;var foundfn:string):boolean;

  function CfgFileExists(const fn:string):boolean;
  begin
    Comment(V_Tried,'Configfile search: '+fn);
    CfgFileExists:=FileExists(fn);
  end;

var
  configpath : pathstr;
begin
  foundfn:=fn;
  check_configfile:=true;
  { retrieve configpath }
{$ifdef Delphi}
  configpath:=FixPath(dmisc.getenv('PPC_CONFIG_PATH'),false);
{$else Delphi}
  configpath:=FixPath(dos.getenv('PPC_CONFIG_PATH'),false);
{$endif Delphi}
{$ifdef Unix}
  if configpath='' then
   configpath:='/etc/';
{$endif}
  {
    Order to read configuration file :
    try reading fpc.cfg in :
     1 - current dir
     2 - configpath
     3 - compiler path
  }
  if not FileExists(fn) then
   begin
{$ifdef Unix}
     if (dos.getenv('HOME')<>'') and CfgFileExists(FixPath(dos.getenv('HOME'),false)+'.'+fn) then
      foundfn:=FixPath(dos.getenv('HOME'),false)+'.'+fn
     else
{$endif}
      if CfgFileExists(configpath+fn) then
       foundfn:=configpath+fn
     else
{$ifndef Unix}
      if CfgFileExists(exepath+fn) then
       foundfn:=exepath+fn
     else
{$endif}
      check_configfile:=false;
   end;
end;


procedure read_arguments(cmd:string);
begin
  option:=coption.create;
  disable_configfile:=false;

{ default defines }
  def_symbol(upper(target_info.shortname));
  def_symbol('FPC');
  def_symbol('VER'+version_nr);
  def_symbol('VER'+version_nr+'_'+release_nr);
  def_symbol('VER'+version_nr+'_'+release_nr+'_'+patch_nr);

{ Temporary defines, until things settle down }
  def_symbol('HASWIDECHAR');
  def_symbol('HASWIDESTRING');
  def_symbol('HASOUT');
  { for now, the PowerPC doesn't support variants and interfaces }
{$ifdef i386}
  def_symbol('HASINTF');
  def_symbol('HASVARIANT');
{$endif i386}
  def_symbol('INTERNSETLENGTH');
  def_symbol('INTERNLENGTH');
  def_symbol('INTERNCOPY');
  def_symbol('INT64FUNCRESOK');
  def_symbol('HAS_ADDR_STACK_ON_STACK');
  def_symbol('NOBOUNDCHECK');
  def_symbol('HASCOMPILERPROC');
  def_symbol('VALUEGETMEM');
  def_symbol('VALUEFREEMEM');
  def_symbol('HASCURRENCY');
  def_symbol('HASTHREADVAR');

{ using a case is pretty useless here (FK) }
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
{$ifdef iA64}
  def_symbol('CPUIA64');
{$endif}
{$ifdef x64_64}
  def_symbol('CPU86_64');
{$endif}
{$ifdef sparc}
  def_symbol('CPUSPARC');
{$endif}
{$ifdef vis}
  def_symbol('CPUVIS');
{$endif}

{ get default messagefile }
{$ifdef Delphi}
  msgfilename:=dmisc.getenv('PPC_ERROR_FILE');
{$else Delphi}
  msgfilename:=dos.getenv('PPC_ERROR_FILE');
{$endif Delphi}

   { default configfile can be specified on the commandline,
     remove it first }
   if (cmd<>'') and (cmd[1]='[') then
    begin
      ppccfg:=Copy(cmd,2,pos(']',cmd)-2);
      Delete(cmd,1,pos(']',cmd));
    end
   else
    begin
      ppccfg:='fpc.cfg';
      ppcaltcfg:='ppc386.cfg';
    end;

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
   option.firstpass:=false;

  { read configuration file }
  if (not disable_configfile) and
     (ppccfg<>'') then
    begin
      read_configfile:=check_configfile(ppccfg,ppccfg);
      { Maybe alternative configfile ? }
      if (not read_configfile) and
         (ppcaltcfg<>'') then
        read_configfile:=check_configfile(ppcaltcfg,ppccfg);
    end
  else
    read_configfile := false;

{ Read commandline and configfile }
  target_is_set:=false;
  asm_is_set:=false;
  param_file:='';

  { read configfile }
  if read_configfile then
    option.interpret_file(ppccfg);

  { read parameters again to override config file }
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
  Option.TargetDefines(true);

  { endian define }
  case target_info.endian of
    endian_little :
      def_symbol('ENDIAN_LITTLE');
    endian_big :
      def_symbol('ENDIAN_BIG');
  end;

{$ifdef m68k}
  if initoptprocessor=MC68020 then
    def_symbol('CPUM68020');
{$endif m68k}

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
   begin
{$ifdef Delphi}
     UnitSearchPath.AddPath(dmisc.getenv(target_info.unit_env),false);
{$else}
     UnitSearchPath.AddPath(dos.getenv(target_info.unit_env),false);
{$endif Delphi}
   end;

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

  if (target_asm.supported_target <> system_any) and
     (target_asm.supported_target <> target_info.system) then
   begin
     Message2(option_incompatible_asm,target_asm.idtxt,target_info.name);
     set_target_asm(target_info.assemextern);
     Message1(option_asm_forced,target_asm.idtxt);
   end;

{ turn off stripping if compiling with debuginfo or profile }
  if (cs_debuginfo in initmoduleswitches) or
     (cs_profile in initmoduleswitches) then
    exclude(initglobalswitches,cs_link_strip);

  if not LinkTypeSetExplicitly then
   set_default_link_type;

  { Default alignment settings,
    1. load the defaults for the target
    2. override with generic optimizer setting (little size)
    3. override with the user specified -Oa }
  UpdateAlignment(initalignment,target_info.alignment);
  if (cs_littlesize in aktglobalswitches) then
   begin
     initalignment.procalign:=1;
     initalignment.jumpalign:=1;
     initalignment.loopalign:=1;
   end;
  UpdateAlignment(initalignment,option.paraalignment);

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
  Revision 1.90  2002-11-30 23:14:55  carl
    - removed cs_fp_emulation checking for m68k, its now controled
      by a global switch
    + added powerpc/sparc/vis message options support

  Revision 1.89  2002/11/30 21:29:56  carl
    + -Ce for softfpu

  Revision 1.88  2002/11/15 01:58:52  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.87  2002/10/23 17:07:40  peter
    * fix -n that was broken in the previous commit

  Revision 1.86  2002/10/23 16:57:16  peter
    * first search for fpc.cfg instead of deprecated ppc386.cfg
    * parse commandline options first before searching configfile so -vt
      can be used to display the searched files

  Revision 1.85  2002/10/13 21:33:01  peter
    * define HASTHREADVAR

  Revision 1.84  2002/10/02 18:20:52  peter
    * Copy() is now internal syssym that calls compilerprocs

  Revision 1.83  2002/09/22 14:02:35  carl
    * stack checking cannot be called before system unit is initialized
    * MC68020 define

  Revision 1.82  2002/08/12 15:08:40  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.81  2002/08/10 14:46:29  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.80  2002/08/09 19:15:41  carl
     - removed newcg define

  Revision 1.79  2002/07/26 22:22:10  florian
    * several PowerPC related fixes to get forward with system unit compilation

  Revision 1.78  2002/07/26 21:15:39  florian
    * rewrote the system handling

  Revision 1.77  2002/07/20 17:16:03  florian
    + source code page support

  Revision 1.76  2002/07/04 20:43:01  florian
    * first x86-64 patches

  Revision 1.75  2002/07/01 18:46:24  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.74  2002/07/01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.73  2002/05/18 13:34:11  peter
    * readded missing revisions

  Revision 1.72  2002/05/16 19:46:41  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.70  2002/05/12 16:53:08  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.69  2002/04/21 19:02:04  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.68  2002/04/20 21:32:24  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.67  2002/04/07 10:22:35  carl
  + CPU defines now depends on current target

  Revision 1.66  2002/04/04 19:05:58  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.65  2002/04/04 18:39:45  carl
  + added wdosx support (patch from Pavel)

}
