{
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
  CClasses,CFileUtils,
  globtype,globals,verbose,systems,cpuinfo, comprsrc;

Type
  TOption=class
    FirstPass,
    ParaLogo,
    NoPressEnter,
    LogoWritten : boolean;
    FileLevel : longint;
    QuickInfo : string;
    ParaIncludePath,
    ParaUnitPath,
    ParaObjectPath,
    ParaLibraryPath,
    ParaFrameworkPath : TSearchPathList;
    ParaAlignment   : TAlignmentInfo;
    Constructor Create;
    Destructor Destroy;override;
    procedure WriteLogo;
    procedure WriteInfo;
    procedure WriteHelpPages;
    procedure WriteQuickInfo;
    procedure IllegalPara(const opt:string);
    function  Unsetbool(var Opts:TCmdStr; Pos: Longint):boolean;
    procedure interpret_option(const opt :string;ispara:boolean);
    procedure Interpret_envvar(const envname : string);
    procedure Interpret_file(const filename : string);
    procedure Read_Parameters;
    procedure parsecmd(cmd:string);
    procedure TargetOptions(def:boolean);
  end;

  TOptionClass=class of toption;

var
  coption : TOptionClass;

procedure read_arguments(cmd:string);


implementation

uses
  widestr,
  charset,
  SysUtils,
  version,
  cutils,cmsgs,
  comphook,
  symtable,scanner,rabase
  ;

const
  page_size = 24;

var
  option     : toption;
  read_configfile,        { read config file, set when a cfgfile is found }
  disable_configfile : boolean;
  fpcdir,
  ppccfg,
  ppcaltcfg,
  param_file    : string;   { file to compile specified on the commandline }

{****************************************************************************
                                 Defines
****************************************************************************}

procedure set_default_link_type;
begin
  undef_system_macro('FPC_LINK_SMART');
  def_system_macro('FPC_LINK_STATIC');
  undef_system_macro('FPC_LINK_DYNAMIC');
  init_settings.globalswitches:=init_settings.globalswitches+[cs_link_static];
  init_settings.globalswitches:=init_settings.globalswitches-[cs_link_shared,cs_link_smart];
end;

{****************************************************************************
                                 Toption
****************************************************************************}

procedure StopOptions(err:longint);
begin
  if assigned(Option) then
   begin
     Option.free;
     Option:=nil;
   end;
  raise ECompilerAbortSilent.Create;
end;


procedure Toption.WriteLogo;
var
  p : pchar;
begin
  if not LogoWritten then
    begin
      p:=MessagePchar(option_logo);
      while assigned(p) do
        Comment(V_Normal,GetMsgLine(p));
      LogoWritten:= true;
    end;
end;


procedure Toption.WriteInfo;
var
  p : pchar;
  hs,hs1,s : TCmdStr;
  target : tsystem;
  cpu : tcputype;
  fpu : tfputype;
  opt : toptimizerswitch;
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
     else if pos('$INSTRUCTIONSETS',s)>0 then
      begin
        for cpu:=low(tcputype) to high(tcputype) do
          begin
            hs:=s;
            hs1:=cputypestr[cpu];
            if hs1<>'' then
              begin
                Replace(hs,'$INSTRUCTIONSETS',hs1);
                Comment(V_Normal,hs);
              end;
          end;
      end
     else if pos('$FPUINSTRUCTIONSETS',s)>0 then
      begin
        for fpu:=low(tfputype) to high(tfputype) do
          begin
            hs:=s;
            hs1:=fputypestr[fpu];
            if hs1<>'' then
              begin
                Replace(hs,'$FPUINSTRUCTIONSETS',hs1);
                Comment(V_Normal,hs);
              end;
          end;
      end
     else if pos('$OPTIMIZATIONS',s)>0 then
      begin
        for opt:=low(toptimizerswitch) to high(toptimizerswitch) do
          begin
            if opt in supported_optimizerswitches then
              begin
                hs:=s;
                hs1:=OptimizerSwitchStr[opt];
                if hs1<>'' then
                  begin
                    Replace(hs,'$OPTIMIZATIONS',hs1);
                    Comment(V_Normal,hs);
                  end;
              end;
          end;
      end
     else
      Comment(V_Normal,s);
   end;
  StopOptions(0);
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
  Message1(option_usage,FixFileName(system.paramstr(0)));
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
{$ifdef x86_64}
      '4',
{$endif}
{$ifdef m68k}
      '6',
{$endif}
{$ifdef arm}
      'A',
{$endif}
{$ifdef powerpc}
      'P',
{$endif}
{$ifdef powerpc64}
      'p',
{$endif}
{$ifdef sparc}
      'S',
{$endif}
{$ifdef vis}
      'V',
{$endif}
      '*' : show:=true;
     end;
     if show then
      begin
        case s[2] of
         'g',
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
                 outline:=11;
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
               StopOptions(0);
            end;
           lines:=0;
         end;
        Comment(V_Normal,PadEnd('',ident)+opt+Copy(s,j+1,255));
        LastIdent:=Ident;
        inc(Lines);
      end;
   end;
  StopOptions(0);
end;


procedure Toption.IllegalPara(const opt:string);
begin
  Message1(option_illegal_para,opt);
  Message(option_help_pages_para);
  StopOptions(1);
end;


function Toption.Unsetbool(var Opts:TCmdStr; Pos: Longint):boolean;
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


procedure TOption.interpret_option(const opt:string;ispara:boolean);
var
  code : integer;
  c    : char;
  more : TCmdStr;
  major,minor : longint;
  error : integer;
  j,l   : longint;
  d,s   : TCmdStr;
  unicodemapping : punicodemap;
begin
  if opt='' then
   exit;

  { only parse define,undef,target,verbosity,link etc options the firsttime }
  if firstpass and
     not(
         (opt[1]='-') and
         (
          ((length(opt)>1) and (opt[2] in ['i','d','v','T','u','n','X','l'])) or
          ((length(opt)>3) and (opt[2]='F') and (opt[3]='e'))
         )
        ) then
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
               include(init_settings.globalswitches,cs_asm_leave);
               j:=1;
               while j<=length(more) do
                begin
                  case more[j] of
                    'l' :
                      include(init_settings.globalswitches,cs_asm_source);
                    'r' :
                      include(init_settings.globalswitches,cs_asm_regalloc);
                    't' :
                      include(init_settings.globalswitches,cs_asm_tempalloc);
                    'n' :
                      include(init_settings.globalswitches,cs_asm_nodes);
                    'p' :
                      begin
                        exclude(init_settings.globalswitches,cs_asm_leave);
                        if UnsetBool(More, 0) then
                          exclude(init_settings.globalswitches,cs_asm_pipe)
                        else
                          include(init_settings.globalswitches,cs_asm_pipe);
                      end;
                    '-' :
                      init_settings.globalswitches:=init_settings.globalswitches -
                          [cs_asm_leave, cs_asm_source,cs_asm_regalloc, cs_asm_tempalloc,
                           cs_asm_nodes, cs_asm_pipe];
                    else
                      IllegalPara(opt);
                  end;
                  inc(j);
                end;
             end;

           'A' :
             begin
               paratargetasm:=find_asm_by_string(More);
               if paratargetasm=as_none then
                 IllegalPara(opt);
             end;

           'b' :
             begin
               // Message1(option_obsolete_switch,'-b');
               if UnsetBool(More,0) then
                 begin
                   init_settings.moduleswitches:=init_settings.moduleswitches-[cs_browser];
                   init_settings.moduleswitches:=init_settings.moduleswitches-[cs_local_browser];
                 end
               else
                 begin
                   init_settings.moduleswitches:=init_settings.moduleswitches+[cs_browser];
                 end;
               if More<>'' then
                 if (More='l') or (More='l+') then
                   init_settings.moduleswitches:=init_settings.moduleswitches+[cs_local_browser]
                 else if More='l-' then
                   init_settings.moduleswitches:=init_settings.moduleswitches-[cs_local_browser]
                 else
                   IllegalPara(opt);
             end;

           'B' :
             do_build:=not UnSetBool(more,0);

           'C' :
             begin
               j:=1;
               while j<=length(more) do
                begin
                  case more[j] of
                    'c' :
                       begin
                         if not SetAktProcCall(upper(copy(more,j+1,length(more)-j)),init_settings.defproccall) then
                          IllegalPara(opt);
                         break;
                       end;
{$ifdef cpufpemu}
                    'e' :
                       begin
                         If UnsetBool(More, j) then
                           exclude(init_settings.moduleswitches,cs_fp_emulation)
                         Else
                           include(init_settings.moduleswitches,cs_fp_emulation);
                       end;
{$endif cpufpemu}
                    'f' :
                      begin
                        s:=upper(copy(more,j+1,length(more)-j));
                        if not(SetFpuType(s,init_settings.fputype)) then
                          IllegalPara(opt);
                        break;
                      end;
                    'F' :
                       begin
                         if not SetMinFPConstPrec(copy(more,j+1,length(more)-j),init_settings.minfpconstprec) then
                           IllegalPara(opt);
                         break;
                       end;
                    'g' :
                       if tf_no_pic_supported in target_info.flags then
                         message(scan_w_pic_ignored)
                       else if UnsetBool(More, j) then
                         exclude(init_settings.moduleswitches,cs_create_pic)
                       else
                         include(init_settings.moduleswitches,cs_create_pic);
                    'h' :
                      begin
                         val(copy(more,j+1,length(more)-j),heapsize,code);
                         if (code<>0) or (heapsize<1024) then
                           IllegalPara(opt);
                         break;
                      end;
                    'i' :
                      If UnsetBool(More, j) then
                        exclude(init_settings.localswitches,cs_check_io)
                      else
                        include(init_settings.localswitches,cs_check_io);
                    'n' :
                      If UnsetBool(More, j) then
                        exclude(init_settings.globalswitches,cs_link_nolink)
                      Else
                        include(init_settings.globalswitches,cs_link_nolink);
                    'o' :
                      If UnsetBool(More, j) then
                        exclude(init_settings.localswitches,cs_check_overflow)
                      Else
                        include(init_settings.localswitches,cs_check_overflow);
                    'O' :
                      If UnsetBool(More, j) then
                        exclude(init_settings.localswitches,cs_check_ordinal_size)
                      Else
                        include(init_settings.localswitches,cs_check_ordinal_size);
                    'p' :
                      begin
                        s:=upper(copy(more,j+1,length(more)-j));
                        if not(Setcputype(s,init_settings.cputype)) then
                          IllegalPara(opt);
                        break;
                      end;
                    'P':
                      begin
                        delete(more,1,1);
                        if upper(copy(more,1,pos('=',more)-1))='PACKSET' then
                          begin
                            delete(more,1,pos('=',more));
                            if more='0' then
                              init_settings.setalloc:=0
                            else if (more='1') or (more='DEFAULT') or (more='NORMAL') then
                              init_settings.setalloc:=1
                            else if more='2' then
                              init_settings.setalloc:=2
                            else if more='4' then
                              init_settings.setalloc:=4
                            else if more='8' then
                              init_settings.setalloc:=8
                            else
                              IllegalPara(opt);
                          end
                        else
                          IllegalPara(opt);
                      end;
                    'r' :
                      If UnsetBool(More, j) then
                        exclude(init_settings.localswitches,cs_check_range)
                      Else
                        include(init_settings.localswitches,cs_check_range);
                    'R' :
                      If UnsetBool(More, j) then
                        begin
                          exclude(init_settings.localswitches,cs_check_range);
                          exclude(init_settings.localswitches,cs_check_object);
                        end
                      Else
                        begin
                          include(init_settings.localswitches,cs_check_range);
                          include(init_settings.localswitches,cs_check_object);
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
                         exclude(init_settings.localswitches,cs_check_stack)
                       Else
                         include(init_settings.localswitches,cs_check_stack);
                    'D' :
                       If UnsetBool(More, j) then
                         exclude(init_settings.moduleswitches,cs_create_dynamic)
                       Else
                         include(init_settings.moduleswitches,cs_create_dynamic);
                    'X' :
                       If UnsetBool(More, j) then
                         exclude(init_settings.moduleswitches,cs_create_smart)
                       Else
                         include(init_settings.moduleswitches,cs_create_smart);
                    else
                       IllegalPara(opt);
                  end;
                  inc(j);
                end;
             end;

           'd' :
             if more <> '' then
               begin
                 l:=Pos(':=',more);
                 if l>0 then
                   set_system_compvar(Copy(more,1,l-1),Copy(more,l+2,255))
                 else
                   def_system_macro(more);
               end;
           'D' :
             begin
               include(init_settings.globalswitches,cs_link_deffile);
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
                           val(copy(dllversion,l+1,255),minor,error);
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
                          val(copy(dllversion,1,l-1),major,error);
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
                        exclude(init_settings.globalswitches,cs_link_deffile);
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
                 exclude(init_settings.globalswitches,cs_link_nolink)
               else
                 include(init_settings.globalswitches,cs_link_nolink);
             end;

           'f' :
             begin
               if more='PIC' then
                 begin
                   if tf_no_pic_supported in target_info.flags then
                     message(scan_w_pic_ignored)
                   else
                     include(init_settings.moduleswitches,cs_create_pic)
                 end
               else
                 IllegalPara(opt);
             end;

           'F' :
             begin
               if more='' then
                 IllegalPara(opt);
               c:=more[1];
               Delete(more,1,1);
               DefaultReplacements(More);
               case c of
                 'a' :
                   autoloadunits:=more;
                 'c' :
                   begin
                     if (upper(more)='UTF8') or (upper(more)='UTF-8') then
                        init_settings.sourcecodepage:='utf8'
                     else if not(cpavailable(more)) then
                       Message1(option_code_page_not_available,more)
                     else
                       init_settings.sourcecodepage:=more;
                   end;
                 'C' :
                   RCCompiler := More;
                 'D' :
                   utilsdirectory:=FixPath(More,true);
                 'e' :
                   SetRedirectFile(More);
                 'E' :
                   OutputExeDir:=FixPath(More,true);
                 'f' :
                     if (target_info.system in systems_darwin) then
                       if ispara then
                         ParaFrameworkPath.AddPath(More,false)
                       else
                         frameworksearchpath.AddPath(More,true)
                     else
                       IllegalPara(opt);
                 'i' :
                   begin
                     if ispara then
                       ParaIncludePath.AddPath(More,false)
                     else
                       includesearchpath.AddPath(More,true);
                   end;
                 'm' :
                   begin
                     unicodemapping:=loadunicodemapping(More,More+'.txt');
                     if assigned(unicodemapping) then
                       registermapping(unicodemapping)
                     else
                       IllegalPara(opt);
                   end;
                 'g' :
                   Message2(option_obsolete_switch_use_new,'-Fg','-Fl');
                 'l' :
                   begin
                     if ispara then
                       ParaLibraryPath.AddPath(sysrootpath,More,false)
                     else
                       LibrarySearchPath.AddPath(sysrootpath,More,true);
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
                 'R' :
                   ResCompiler := More;
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

           'g' :
             begin
               if UnsetBool(More, 0) then
                begin
                  exclude(init_settings.moduleswitches,cs_debuginfo);
                  exclude(init_settings.globalswitches,cs_use_heaptrc);
                  exclude(init_settings.globalswitches,cs_use_lineinfo);
                  exclude(init_settings.localswitches,cs_checkpointer);
                  localvartrashing := -1;
                end
               else
                begin
                  include(init_settings.moduleswitches,cs_debuginfo);
                  if paratargetdbg=dbg_none then
                    paratargetdbg:=target_info.dbg;
                end;
               if not RelocSectionSetExplicitly then
                 RelocSection:=false;
               j:=1;
               while j<=length(more) do
                 begin
                   case more[j] of
                     'c' :
                       begin
                         if UnsetBool(More, j) then
                           exclude(init_settings.localswitches,cs_checkpointer)
                         else
                           include(init_settings.localswitches,cs_checkpointer);
                       end;
                     'h' :
                       begin
                         if UnsetBool(More, j) then
                           exclude(init_settings.globalswitches,cs_use_heaptrc)
                         else
                           include(init_settings.globalswitches,cs_use_heaptrc);
                       end;
                     'l' :
                       begin
                         if UnsetBool(More, j) then
                           exclude(init_settings.globalswitches,cs_use_lineinfo)
                         else
                           include(init_settings.globalswitches,cs_use_lineinfo);
                       end;
                     'o' :
                       begin
                         if not UpdateDebugStr(copy(more,j+1,length(more)),init_settings.debugswitches) then
                           IllegalPara(opt);
                         break;
                       end;
                     'p' :
                       begin
                         if UnsetBool(More, j) then
                           exclude(init_settings.globalswitches,cs_stabs_preservecase)
                         else
                           include(init_settings.globalswitches,cs_stabs_preservecase);
                       end;
                     's' :
                       begin
                         paratargetdbg:=dbg_stabs;
                       end;
                     't' :
                       begin
                         if UnsetBool(More, j) then
                            localvartrashing := -1
                         else
                           localvartrashing := (localvartrashing + 1) mod nroftrashvalues;
                       end;
                     'v' :
                       begin
                         if UnsetBool(More, j) then
                           exclude(init_settings.globalswitches,cs_gdb_valgrind)
                         else
                           include(init_settings.globalswitches,cs_gdb_valgrind);
                       end;
                     'w' :
                       begin
                         if (j<length(more)) and (more[j+1] in ['2','3']) then
                           begin
                             case more[j+1] of
                               '2': paratargetdbg:=dbg_dwarf2;
                               '3': paratargetdbg:=dbg_dwarf3;
                             end;
                             inc(j);
                           end
                         else
                           paratargetdbg:=dbg_dwarf2;
                       end;
                     else
                       IllegalPara(opt);
                   end;
                   inc(j);
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
             ParaLogo:=not UnSetBool(more,0);

           'm' :
             parapreprocess:=not UnSetBool(more,0);

           'M' :
             begin
               more:=Upper(more);
               if not SetCompileMode(more, true) then
                 IllegalPara(opt);
             end;

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
                 begin
                   DefaultReplacements(More);
                   D:=ExtractFilePath(More);
                   if (D<>'') then
                     OutputExeDir:=FixPath(D,True);
                   OutputFileName:=ExtractFileName(More);
                 end
               else
                 IllegalPara(opt);
             end;

           'O' :
             begin
               j:=1;
               while j<=length(more) do
                begin
                  case more[j] of
                    '1' :
                      init_settings.optimizerswitches:=init_settings.optimizerswitches+level1optimizerswitches;
                    '2' :
                      init_settings.optimizerswitches:=init_settings.optimizerswitches+level2optimizerswitches;
                    '3' :
                      init_settings.optimizerswitches:=init_settings.optimizerswitches+level3optimizerswitches;
                    'a' :
                      begin
                        if not(UpdateAlignmentStr(Copy(Opt,j+3,255),ParaAlignment)) then
                          IllegalPara(opt);
                        break;
                      end;
                    's' :
                      include(init_settings.optimizerswitches,cs_opt_size);
                    'p' :
                      begin
                        if not Setcputype(copy(more,j+1,length(more)),init_settings.optimizecputype) then
                          begin
                            { Give warning for old i386 switches }
                            if (Length(More)-j=1) and
                               (More[j+1]>='1') and (More[j+1]<='5')then
                              Message2(option_obsolete_switch_use_new,'-Op<nr>','-Op<name>')
                            else
                              IllegalPara(opt);
                          end;
                        break;
                      end;
                    'o' :
                      begin
                        if not UpdateOptimizerStr(copy(more,j+1,length(more)),init_settings.optimizerswitches) then
                         IllegalPara(opt);
                        break;
                      end;
                    '-' :
                      begin
                        init_settings.optimizerswitches:=[];
                        FillChar(ParaAlignment,sizeof(ParaAlignment),0);
                      end;
                    { Obsolete switches }
                    'g' :
                      Message2(option_obsolete_switch_use_new,'-Og','-Os');
                    'G' :
                      Message1(option_obsolete_switch,'-OG');
                    'r' :
                      Message2(option_obsolete_switch_use_new,'-Or','-O2 or -Ooregvar');
                    'u' :
                      Message2(option_obsolete_switch_use_new,'-Ou','-Oouncertain');
                    else
                      IllegalPara(opt);
                  end;
                  inc(j);
                end;
             end;

           'p' :
             begin
               if UnsetBool(More, 0) then
                 begin
                   init_settings.moduleswitches:=init_settings.moduleswitches-[cs_profile];
                   undef_system_macro('FPC_PROFILE');
                 end
               else
                 if Length(More)=0 then
                   IllegalPara(opt)
                 else
                 case more[1] of
                  'g' : if UnsetBool(more, 1) then
                         begin
                           exclude(init_settings.moduleswitches,cs_profile);
                           undef_system_macro('FPC_PROFILE');
                         end
                        else
                         begin
                           include(init_settings.moduleswitches,cs_profile);
                           def_system_macro('FPC_PROFILE');
                        end;
                 else
                   IllegalPara(opt);
                 end;
             end;

           'P' : ; { Ignore used by fpc.pp }

           'R' :
             begin
               if not SetAsmReadMode(More,init_settings.asmmode) then
                 IllegalPara(opt);
             end;

           's' :
             begin
               if UnsetBool(More, 0) then
                 begin
                   init_settings.globalswitches:=init_settings.globalswitches-[cs_asm_extern,cs_link_extern,cs_link_nolink];
                   if more<>'' then
                     IllegalPara(opt);
                 end
               else
                 begin
                   init_settings.globalswitches:=init_settings.globalswitches+[cs_asm_extern,cs_link_extern,cs_link_nolink];
                   if more='h' then
                     init_settings.globalswitches:=init_settings.globalswitches-[cs_link_on_target]
                   else if more='t' then
                     init_settings.globalswitches:=init_settings.globalswitches+[cs_link_on_target]
                   else if more='r' then
                     init_settings.globalswitches:=init_settings.globalswitches+[cs_asm_leave,cs_no_regalloc]
                   else if more<>'' then
                     IllegalPara(opt);
                 end;
             end;

           'S' :
             begin
               if more='' then
                 IllegalPara(opt);
               if more[1]='I' then
                 begin
                   if upper(more)='ICOM' then
                     init_settings.interfacetype:=it_interfacecom
                   else if upper(more)='ICORBA' then
                     init_settings.interfacetype:=it_interfacecorba
                   else
                     IllegalPara(opt);
                 end
               else
                begin
                  j:=1;
                  while j<=length(more) do
                   begin
                     case more[j] of
                       '2' : //an alternative to -Mobjfpc
                         SetCompileMode('OBJFPC',true);
                       'a' :
                         If UnsetBool(More, j) then
                           exclude(init_settings.localswitches,cs_do_assertion)
                         else
                           include(init_settings.localswitches,cs_do_assertion);
                       'c' :
                         If UnsetBool(More, j) then
                           include(init_settings.moduleswitches,cs_support_c_operators)
                         else
                           include(init_settings.moduleswitches,cs_support_c_operators);
                       'd' : //an alternative to -Mdelphi
                         SetCompileMode('DELPHI',true);
                       'e' :
                         begin
                           SetErrorFlags(copy(more,j+1,length(more)));
                           break;
                         end;
                       'f' :
                         begin
                           inc(j);
                           if more[j]='-' then
                             begin
                               features:=[];
                               if length(more)>j then
                                 IllegalPara(opt);
                             end
                           else
                             begin
                               if not(IncludeFeature(upper(copy(more,j,length(more)-j+1)))) then
                                 IllegalPara(opt);
                             end;
                         end;
                       'g' :
                         If UnsetBool(More, j) then
                           exclude(init_settings.moduleswitches,cs_support_goto)
                         else
                           include(init_settings.moduleswitches,cs_support_goto);
                       'h' :
                         If UnsetBool(More, j) then
                           exclude(init_settings.localswitches,cs_ansistrings)
                         else
                           include(init_settings.localswitches,cs_ansistrings);
                       'i' :
                         If UnsetBool(More, j) then
                           exclude(init_settings.localswitches,cs_do_inline)
                         else
                           include(init_settings.localswitches,cs_do_inline);
                       'k' :
                         If UnsetBool(More, j) then
                           exclude(init_settings.globalswitches,cs_load_fpcylix_unit)
                         else
                           include(init_settings.globalswitches,cs_load_fpcylix_unit);
                       'm' :
                         If UnsetBool(More, j) then
                           exclude(init_settings.moduleswitches,cs_support_macro)
                         else
                           include(init_settings.moduleswitches,cs_support_macro);
                       'o' : //an alternative to -Mtp
                         SetCompileMode('TP',true);
{$ifdef gpc_mode}
                       'p' : //an alternative to -Mgpc
                         SetCompileMode('GPC',true);
{$endif}
                       's' :
                         If UnsetBool(More, j) then
                           exclude(init_settings.globalswitches,cs_constructor_name)
                         else
                           include(init_settings.globalswitches,cs_constructor_name);
                       't' :
                         If UnsetBool(More, j) then
                           exclude(init_settings.moduleswitches,cs_static_keyword)
                         else
                           include(init_settings.moduleswitches,cs_static_keyword);
                       'v' :
                         If UnsetBool(More, j) then
                           exclude(init_settings.globalswitches,cs_support_vectors)
                         else
                           include(init_settings.globalswitches,cs_support_vectors);
                       'x' :
                         If UnsetBool(More, j) then
                           exclude(init_settings.globalswitches,cs_support_exceptions)
                         else
                           include(init_settings.globalswitches,cs_support_exceptions);
                       '-' :
                         begin
                           init_settings.globalswitches:=init_settings.globalswitches - [cs_constructor_name,cs_support_exceptions];
                           init_settings.localswitches:=init_settings.localswitches - [cs_do_assertion, cs_do_inline, cs_ansistrings];
                           init_settings.moduleswitches:=init_settings.moduleswitches - [cs_support_c_operators, cs_support_goto,
                                                                     cs_support_macro,
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
               if paratarget=system_none then
                begin
                  { remove old target define }
                  TargetOptions(false);
                  { load new target }
                  paratarget:=find_system_by_string(More);
                  if paratarget<>system_none then
                    set_target(paratarget)
                  else
                    IllegalPara(opt);
                  { set new define }
                  TargetOptions(true);
                end
               else
                if More<>upper(target_info.shortname) then
                 Message1(option_target_is_already_set,target_info.shortname);
             end;

           'u' :
             if more <> '' then
               undef_system_macro(more);
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
                      exclude(init_settings.globalswitches,cs_check_unit_name);
                    'p' :
                       begin
                         Message2(option_obsolete_switch_use_new,'-Up','-Fu');
                         break;
                       end;
                    'r' :
                      do_release:=true;
                    's' :
                      include(init_settings.moduleswitches,cs_compilesystem);
                    '-' :
                      begin
                        exclude(init_settings.moduleswitches,cs_compilesystem);
                        exclude(init_settings.globalswitches,cs_check_unit_name);
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

           'V' : ; { Ignore used by fpc }

           'W' :
             begin
               j:=1;
               while j<=length(More) do
                begin
                  case More[j] of
                    'A':
                      begin
                        if UnsetBool(More, j) then
                          apptype:=app_native
                        else
                          apptype:=app_cui;
                      end;
                    'b':
                      begin
                        if (target_info.system in systems_darwin) then
                          begin
                            if not UnsetBool(More, j) then
                              apptype:=app_bundle
                            else
                              apptype:=app_cui
                          end
                        else
                          IllegalPara(opt);
                      end;
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
                      begin
                        UseDeffileForExports:=not UnsetBool(More, j);
                        UseDeffileForExportsSetExplicitly:=true;
                      end;
                    'F':
                      begin
                        if UnsetBool(More, j) then
                          apptype:=app_cui
                        else
                          apptype:=app_fs;
                      end;
                    'G':
                      begin
                        if UnsetBool(More, j) then
                          apptype:=app_cui
                        else
                          apptype:=app_gui;
                      end;
                    'I':
                      begin
                        GenerateImportSection:=not UnsetBool(More,j);
                        GenerateImportSectionSetExplicitly:=true;
                      end;
                    'N':
                      begin
                        RelocSection:=UnsetBool(More,j);
                        RelocSectionSetExplicitly:=true;
                      end;
                    'R':
                      begin
                        { support -WR+ / -WR- as synonyms to -WR / -WN }
                        RelocSection:=not UnsetBool(More,j);
                        RelocSectionSetExplicitly:=true;
                      end;
                    'T':
                      begin
                        if UnsetBool(More, j) then
                          apptype:=app_cui
                        else
                          apptype:=app_tool;
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
                    'c' : Cshared:=TRUE;
                    'd' : Dontlinkstdlibpath:=TRUE;
                    'e' :
                      begin
                        If UnsetBool(More, j) then
                          exclude(init_settings.globalswitches,cs_link_extern)
                        else
                          include(init_settings.globalswitches,cs_link_extern);
                      end;
                    'f' :
                      include(init_settings.globalswitches,cs_link_pthread);
                    'g' :
                      begin
                        If UnsetBool(More, j) then
                          exclude(init_settings.globalswitches,cs_link_separate_dbg_file)
                        else
                          include(init_settings.globalswitches,cs_link_separate_dbg_file);
                      end;
                    'i' :
                      begin
                        If UnsetBool(More, j) then
                          include(init_settings.globalswitches,cs_link_extern)
                        else
                          exclude(init_settings.globalswitches,cs_link_extern);
                      end;
                    'm' :
                      begin
                        If UnsetBool(More, j) then
                          exclude(init_settings.globalswitches,cs_link_map)
                        else
                          include(init_settings.globalswitches,cs_link_map);
                      end;
                    'p' : ; { Ignore used by fpc.pp }
                    'r' :
                      begin
                        rlinkpath:=Copy(more,2,length(More)-1);
                        DefaultReplacements(rlinkpath);
                        More:='';
                      end;
                    'R' :
                      begin
                        sysrootpath:=copy(more,2,length(more)-1);
                        defaultreplacements(sysrootpath);
                        more:='';
                      end;
                    's' :
                      begin
                        If UnsetBool(More, j) then
                          exclude(init_settings.globalswitches,cs_link_strip)
                        else
                          include(init_settings.globalswitches,cs_link_strip);
                      end;
                    't' :
                      include(init_settings.globalswitches,cs_link_staticflag);
                    'v' :
                      begin
                        If UnsetBool(More, j) then
                          exclude(init_settings.globalswitches,cs_link_opt_vtable)
                        else
                          include(init_settings.globalswitches,cs_link_opt_vtable);
                      end;
                    'D' :
                      begin
                        def_system_macro('FPC_LINK_DYNAMIC');
                        undef_system_macro('FPC_LINK_SMART');
                        undef_system_macro('FPC_LINK_STATIC');
                        exclude(init_settings.globalswitches,cs_link_static);
                        exclude(init_settings.globalswitches,cs_link_smart);
                        include(init_settings.globalswitches,cs_link_shared);
                        LinkTypeSetExplicitly:=true;
                      end;
                    'M' :
                      begin
                        mainaliasname:=Copy(more,2,length(More)-1);
                        More:='';
                      end;
                    'P' :
                      begin
                        utilsprefix:=Copy(more,2,length(More)-1);
                        DefaultReplacements(utilsprefix);
                        More:='';
                      end;
                    'L' : begin  // -XLO is link order -XLA is link alias. -XLD avoids load defaults.
                                 // these are not aggregable.
                            if (j=length(more)) or not (more[j+1] in ['O','A','D']) then
                              IllegalPara(opt)
                            else
                              begin
                                case more[j+1] of
                                 'A' : begin
                                        s:=Copy(more,3,length(More)-2);
                                        if not LinkLibraryAliases.AddDep(s) Then
                                           IllegalPara(opt);
                                       end;
                                 'O' : begin
                                        s:=Copy(more,3,length(More)-2);
                                        if not LinkLibraryOrder.AddWeight(s) Then
                                           IllegalPara(opt);
                                       end;
                                 'D' : include(init_settings.globalswitches,cs_link_no_default_lib_order)
                                else
                                  IllegalPara(opt);
                                 end; {case}
                                j:=length(more);
                              end; {else begin}
                          end;
                    'S' :
                      begin
                        def_system_macro('FPC_LINK_STATIC');
                        undef_system_macro('FPC_LINK_SMART');
                        undef_system_macro('FPC_LINK_DYNAMIC');
                        include(init_settings.globalswitches,cs_link_static);
                        exclude(init_settings.globalswitches,cs_link_smart);
                        exclude(init_settings.globalswitches,cs_link_shared);
                        LinkTypeSetExplicitly:=true;
                      end;
                    'X' :
                      begin
                        def_system_macro('FPC_LINK_SMART');
                        undef_system_macro('FPC_LINK_STATIC');
                        undef_system_macro('FPC_LINK_DYNAMIC');
                        exclude(init_settings.globalswitches,cs_link_static);
                        include(init_settings.globalswitches,cs_link_smart);
                        exclude(init_settings.globalswitches,cs_link_shared);
                        LinkTypeSetExplicitly:=true;
                      end;
                    '-' :
                      begin
                        exclude(init_settings.globalswitches,cs_link_staticflag);
                        exclude(init_settings.globalswitches,cs_link_strip);
                        exclude(init_settings.globalswitches,cs_link_map);
                        set_default_link_type;
                      end;
                    else
                      IllegalPara(opt);
                  end;
                  inc(j);
                end;
             end;
         end;
       end;

    '@' :
      begin
        Message(option_no_nested_response_file);
        StopOptions(1);
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
  s, tmp,
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
{ Maybe It's Directory ?}   //Jaro Change:
  if PathExists(filename,false) then
    begin
       Message1(option_config_is_dir,filename);
       exit;
    end;
{ open file }
  Message1(option_using_file,filename);
  assign(f,ExpandFileName(filename));
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
               skip[level]:=not (assigned(search_macro(s)) or (s='COMMON'));
            end
           else
            if (s='IFDEF') then
             begin
               RemoveSep(opts);
               if Level>=maxlevel then
                begin
                  Message(option_too_many_ifdef);
                  stopOptions(1);
                end;
               inc(Level);
               skip[level]:=(skip[level-1] or not assigned(search_macro(upper(GetName(opts)))));
             end
           else
            if (s='IFNDEF') then
             begin
               RemoveSep(opts);
               if Level>=maxlevel then
                begin
                  Message(option_too_many_ifdef);
                  stopOptions(1);
                end;
               inc(Level);
               skip[level]:=(skip[level-1] or assigned(search_macro(upper(GetName(opts)))));
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
                  stopOptions(1);
                end;
               dec(level);
             end
           else
            if (not skip[level]) then
             begin
               if (s='DEFINE') then
                begin
                  RemoveSep(opts);
                  tmp:= GetName(opts);
                  if tmp <> '' then
                    def_system_macro(tmp);
                end
              else
               if (s='UNDEF') then
                begin
                  RemoveSep(opts);
                  tmp:= GetName(opts);
                  if tmp <> '' then
                    undef_system_macro(tmp);
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
{$warning FIXME: silent truncation of environment parameters }
       if (arglen > 255) then
         arglen := 255;
       setlength(hs,arglen);
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
           'P' :
             addinfo(source_cpu_string);
           else
             IllegalPara('-i'+QuickInfo);
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
             IllegalPara('-i'+QuickInfo);
          end;
        end;
      'V' :
        AddInfo(version_string);
      'W' :
        AddInfo(full_version_string);
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
     stopoptions(0);
   end;
end;


procedure TOption.TargetOptions(def:boolean);
var
  s : string;
  i : integer;
begin
  if def then
   def_system_macro(target_info.shortname)
  else
   undef_system_macro(target_info.shortname);

  s:=target_info.extradefines;
  while (s<>'') do
   begin
     i:=pos(';',s);
     if i=0 then
      i:=length(s)+1;
     if def then
      def_system_macro(Copy(s,1,i-1))
     else
      undef_system_macro(Copy(s,1,i-1));
     delete(s,1,i);
   end;

  { endian define }
  case target_info.endian of
    endian_little :
      begin
         if def then
           begin
             def_system_macro('ENDIAN_LITTLE');
             def_system_macro('FPC_LITTLE_ENDIAN');
           end
         else
           begin
             undef_system_macro('ENDIAN_LITTLE');
             undef_system_macro('FPC_LITTLE_ENDIAN');
           end;
      end;
    endian_big :
      begin
         if def then
           begin
             def_system_macro('ENDIAN_BIG');
             def_system_macro('FPC_BIG_ENDIAN');
           end
         else
           begin
             undef_system_macro('ENDIAN_BIG');
             undef_system_macro('FPC_BIG_ENDIAN');
           end
      end;
  end;

  { abi define }
  case target_info.abi of
    abi_powerpc_sysv :
      if def then
        def_system_macro('FPC_ABI_SYSV')
      else
        undef_system_macro('FPC_ABI_SYSV');
    abi_powerpc_aix :
      if def then
        def_system_macro('FPC_ABI_AIX')
      else
        undef_system_macro('FPC_ABI_AIX');
  end;

  if (tf_winlikewidestring in target_info.flags) then
    if def then
      def_system_macro('FPC_WINLIKEWIDESTRING')
    else
      undef_system_macro('FPC_WINLIKEWIDESTRING');

  if (tf_requires_proper_alignment in target_info.flags) then
    if def then
      def_system_macro('FPC_REQUIRES_PROPER_ALIGNMENT')
    else
      undef_system_macro('FPC_REQUIRES_PROPER_ALIGNMENT');

  if source_info.system<>target_info.system then
    if def then
      def_system_macro('FPC_CROSSCOMPILING')
    else
      undef_system_macro('FPC_CROSSCOMPILING');

  if source_info.cpu<>target_info.cpu then
    if def then
      def_system_macro('FPC_CPUCROSSCOMPILING')
    else
      def_system_macro('FPC_CPUCROSSCOMPILING');

  { Code generation flags }
  if def and
     (tf_pic_default in target_info.flags) then
    include(init_settings.moduleswitches,cs_create_pic)
  else
    exclude(init_settings.moduleswitches,cs_create_pic);
end;


constructor TOption.create;
begin
  LogoWritten:=false;
  NoPressEnter:=false;
  FirstPass:=false;
  FileLevel:=0;
  Quickinfo:='';
  ParaIncludePath:=TSearchPathList.Create;
  ParaObjectPath:=TSearchPathList.Create;
  ParaUnitPath:=TSearchPathList.Create;
  ParaLibraryPath:=TSearchPathList.Create;
  ParaFrameworkPath:=TSearchPathList.Create;
  FillChar(ParaAlignment,sizeof(ParaAlignment),0);
end;


destructor TOption.destroy;
begin
  ParaIncludePath.Free;
  ParaObjectPath.Free;
  ParaUnitPath.Free;
  ParaLibraryPath.Free;
  ParaFrameworkPath.Free;
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
  hs,
  configpath : string;
begin
  foundfn:=fn;
  check_configfile:=true;
  { retrieve configpath }
  configpath:=FixPath(GetEnvironmentVariable('PPC_CONFIG_PATH'),false);
{$ifdef Unix}
  if configpath='' then
   configpath:=ExpandFileName(FixPath(exepath+'../etc/',false));
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
     hs:=GetEnvironmentVariable('HOME');
     if (hs<>'') and CfgFileExists(FixPath(hs,false)+'.'+fn) then
      foundfn:=FixPath(hs,false)+'.'+fn
     else
{$endif}
      if CfgFileExists(configpath+fn) then
       foundfn:=configpath+fn
     else
{$ifndef Unix}
      if CfgFileExists(exepath+fn) then
       foundfn:=exepath+fn
     else
{$else}
      if CfgFileExists('/etc/'+fn) then
       foundfn:='/etc/'+fn
     else
{$endif}
      check_configfile:=false;
   end;
end;


procedure read_arguments(cmd:string);
var
  env: ansistring;
  i : tfeature;
begin
  option:=coption.create;
  disable_configfile:=false;

  { Non-core target defines }
  Option.TargetOptions(true);

{ get default messagefile }
  msgfilename:=GetEnvironmentVariable('PPC_ERROR_FILE');

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

{ first pass reading of parameters, only -i -v -T etc.}
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

{ target is set here, for wince the default app type is gui }
  if target_info.system in system_wince then
    apptype:=app_gui;

{ default defines }
  def_system_macro(target_info.shortname);
  def_system_macro('FPC');
  def_system_macro('VER'+version_nr);
  def_system_macro('VER'+version_nr+'_'+release_nr);
  def_system_macro('VER'+version_nr+'_'+release_nr+'_'+patch_nr);

{ Temporary defines, until things settle down }
  { "main" symbol is generated in the main program, and left out of the system unit }
  def_system_macro('FPC_DARWIN_PASCALMAIN');
  def_system_macro('FPC_DARWIN_JMP_MAIN');
  def_system_macro('PARAOUTFILE');
  def_system_macro('RESSTRSECTIONS');
  def_system_macro('FPC_HASFIXED64BITVARIANT');
  def_system_macro('FPC_HASINTERNALOLEVARIANT2VARIANTCAST');
  def_system_macro('FPC_HAS_VARSETS');
  def_system_macro('FPC_HAS_VALGRINDBOOL');
  def_system_macro('FPC_HAS_STR_CURRENCY');
  def_system_macro('FPC_REAL2REAL_FIXED');
  def_system_macro('FPC_STRTOCHARARRAYPROC');
  def_system_macro('FPC_STRTOSHORTSTRINGPROC');

{$ifdef SUPPORT_UNALIGNED}
  def_system_macro('FPC_SUPPORTS_UNALIGNED');
{$endif SUPPORT_UNALIGNED}
{$ifdef powerpc64}
  def_system_macro('FPC_HAS_LWSYNC');
{$endif}
  def_system_macro('FPC_HAS_MEMBAR');
  def_system_macro('FPC_NEW_BIGENDIAN_SETS');
  def_system_macro('FPC_SETBASE_USED');

{$if defined(x86) or defined(arm)}
  def_system_macro('INTERNAL_BACKTRACE');
{$endif}
  def_system_macro('STR_CONCAT_PROCS');
  if pocall_default = pocall_register then
    def_system_macro('REGCALL');

  { don't remove this, it's also for fpdoc necessary (FK) }
  def_system_macro('FPC_HAS_FEATURE_SUPPORT');
{ using a case is pretty useless here (FK) }
{ some stuff for TP compatibility }
{$ifdef i386}
  def_system_macro('CPU86');
  def_system_macro('CPU87');
  def_system_macro('CPU386');
{$endif}

{ new processor stuff }
{$ifdef i386}
  def_system_macro('CPUI386');
  def_system_macro('CPU32');
  def_system_macro('FPC_HAS_TYPE_EXTENDED');
  def_system_macro('FPC_HAS_TYPE_DOUBLE');
  def_system_macro('FPC_HAS_TYPE_SINGLE');
  def_system_macro('FPC_HAS_RESOURCES');
{$endif}
{$ifdef m68k}
  def_system_macro('CPU68');
  def_system_macro('CPU68K');
  def_system_macro('CPUM68K');
  def_system_macro('CPU32');
  def_system_macro('FPC_HAS_TYPE_DOUBLE');
  def_system_macro('FPC_HAS_TYPE_SINGLE');
  def_system_macro('FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE');
  def_system_macro('FPC_CURRENCY_IS_INT64');
  def_system_macro('FPC_COMP_IS_INT64');
{$endif}
{$ifdef ALPHA}
  def_system_macro('CPUALPHA');
  def_system_macro('CPU64');
{$endif}
{$ifdef powerpc}
  def_system_macro('CPUPOWERPC');
  def_system_macro('CPUPOWERPC32');
  def_system_macro('CPU32');
  def_system_macro('FPC_HAS_TYPE_DOUBLE');
  def_system_macro('FPC_HAS_TYPE_SINGLE');
  def_system_macro('FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE');
  def_system_macro('FPC_CURRENCY_IS_INT64');
  def_system_macro('FPC_COMP_IS_INT64');
{$endif}
{$ifdef POWERPC64}
  def_system_macro('CPUPOWERPC');
  def_system_macro('CPUPOWERPC64');
  def_system_macro('CPU64');
  def_system_macro('FPC_HAS_TYPE_DOUBLE');
  def_system_macro('FPC_HAS_TYPE_SINGLE');
  def_system_macro('FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE');
  def_system_macro('FPC_CURRENCY_IS_INT64');
  def_system_macro('FPC_COMP_IS_INT64');
{$endif}
{$ifdef iA64}
  def_system_macro('CPUIA64');
  def_system_macro('CPU64');
{$endif}
{$ifdef x86_64}
  def_system_macro('CPUX86_64');
  def_system_macro('CPUAMD64');
  def_system_macro('CPU64');
  { not supported for now, afaik (FK)
   def_system_macro('FPC_HAS_TYPE_FLOAT128'); }

  { win64 doesn't support the legacy fpu }
  if target_info.system<>system_x86_64_win64 then
    def_system_macro('FPC_HAS_TYPE_EXTENDED')
  else
    begin
      def_system_macro('FPC_CURRENCY_IS_INT64');
      def_system_macro('FPC_COMP_IS_INT64');
      undef_system_macro('FPC_HAS_TYPE_EXTENDED');
    end;

  def_system_macro('FPC_HAS_TYPE_DOUBLE');
  def_system_macro('FPC_HAS_TYPE_SINGLE');
{$endif}
{$ifdef sparc}
  def_system_macro('CPUSPARC');
  def_system_macro('CPUSPARC32');
  def_system_macro('CPU32');
  def_system_macro('FPC_HAS_TYPE_DOUBLE');
  def_system_macro('FPC_HAS_TYPE_SINGLE');
  def_system_macro('FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE');
  def_system_macro('FPC_CURRENCY_IS_INT64');
  def_system_macro('FPC_COMP_IS_INT64');
{$endif}
{$ifdef vis}
  def_system_macro('CPUVIS');
  def_system_macro('CPU32');
{$endif}
{$ifdef arm}
  def_system_macro('CPUARM');
  def_system_macro('CPU32');
  def_system_macro('FPC_HAS_TYPE_DOUBLE');
  def_system_macro('FPC_HAS_TYPE_SINGLE');
  def_system_macro('FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE');
  def_system_macro('FPC_CURRENCY_IS_INT64');
  def_system_macro('FPC_COMP_IS_INT64');
{$endif arm}

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
  param_file:='';

  { read configfile }
  if read_configfile then
    option.interpret_file(ppccfg);

  { read parameters again to override config file }
  if cmd<>'' then
    option.parsecmd(cmd)
  else
    begin
      { Write help pages if no parameters are passed }
      if (paramcount=0) then
        Option.WriteHelpPages;
      option.read_parameters;
      { Write only quickinfo }
      if option.quickinfo<>'' then
        option.writequickinfo;
    end;
  { Stop if errors in options }
  if ErrorCount>0 then
   StopOptions(1);

  { Write logo }
  if option.ParaLogo then
    option.writelogo;

{ Check file to compile }
  if param_file='' then
   begin
     Message(option_no_source_found);
     StopOptions(1);
   end;
{$ifndef Unix}
  param_file:=FixFileName(param_file);
{$endif not unix}
  inputfilepath:=ExtractFilePath(param_file);
  inputfilename:=ExtractFileName(param_file);
  if ExtractFileExt(inputfilename)='' then
    begin
      if FileExists(inputfilepath+ChangeFileExt(inputfilename,sourceext)) then
        inputfilename:=ChangeFileExt(inputfilename,sourceext)
      else if FileExists(inputfilepath+ChangeFileExt(inputfilename,pasext)) then
        inputfilename:=ChangeFileExt(inputfilename,pasext)
      else if ((m_mac in current_settings.modeswitches) or
              (tf_p_ext_support in target_info.flags))
             and FileExists(inputfilepath+ChangeFileExt(inputfilename,pext)) then
        inputfilename:=ChangeFileExt(inputfilename,pext);
    end;

  { Check output dir }
  if (OutputExeDir<>'') and
     not PathExists(OutputExeDir,false) then
    begin
      Message1(general_e_path_does_not_exist,OutputExeDir);
      StopOptions(1);
    end;

  { Add paths specified with parameters to the searchpaths }
  UnitSearchPath.AddList(option.ParaUnitPath,true);
  ObjectSearchPath.AddList(option.ParaObjectPath,true);
  IncludeSearchPath.AddList(option.ParaIncludePath,true);
  LibrarySearchPath.AddList(option.ParaLibraryPath,true);
  FrameworkSearchPath.AddList(option.ParaFrameworkPath,true);

  { add unit environment and exepath to the unit search path }
  if inputfilepath<>'' then
   Unitsearchpath.AddPath(inputfilepath,true);
  if not disable_configfile then
    begin
      env:=GetEnvironmentVariable(target_info.unit_env);
      if env<>'' then
        UnitSearchPath.AddPath(GetEnvironmentVariable(target_info.unit_env),false);
    end;

{$ifdef Unix}
  fpcdir:=FixPath(GetEnvironmentVariable('FPCDIR'),false);
  if fpcdir='' then
    begin
      if PathExists('/usr/local/lib/fpc/'+version_string,true) then
        fpcdir:='/usr/local/lib/fpc/'+version_string+'/'
      else
        fpcdir:='/usr/lib/fpc/'+version_string+'/';
    end;
{$else unix}
  fpcdir:=FixPath(GetEnvironmentVariable('FPCDIR'),false);
  if fpcdir='' then
    begin
      fpcdir:=ExePath+'../';
      if not(PathExists(fpcdir+'/units',true)) and
         not(PathExists(fpcdir+'/rtl',true)) then
        fpcdir:=fpcdir+'../';
    end;
{$endif unix}
  { first try development RTL, else use the default installation path }
  if not disable_configfile then
    begin
      if PathExists(FpcDir+'rtl',true) then
        if tf_use_8_3 in Source_Info.Flags then
          UnitSearchPath.AddPath(FpcDir+'rtl/'+target_os_string,false)
        else
          UnitSearchPath.AddPath(FpcDir+'rtl/'+target_full_string,false)
      else
        if tf_use_8_3 in Source_Info.Flags then
          UnitSearchPath.AddPath(FpcDir+'units/'+target_os_string+'/rtl',false)
        else
          UnitSearchPath.AddPath(FpcDir+'units/'+target_full_string+'/rtl',false);
    end;
  { Add exepath if the exe is not in the current dir, because that is always searched already.
    Do not add it when linking on the target because then we can maybe already find
    .o files that are not for the target }
  if (ExePath<>GetCurrentDir) and
     not(cs_link_on_target in init_settings.globalswitches) then
   UnitSearchPath.AddPath(ExePath,false);
  { Add unit dir to the object and library path }
  objectsearchpath.AddList(unitsearchpath,false);
  librarysearchpath.AddList(unitsearchpath,false);

  { maybe override assembler }
  if (paratargetasm<>as_none) then
    begin
      if not set_target_asm(paratargetasm) then
        begin
          Message2(option_incompatible_asm,asminfos[paratargetasm]^.idtxt,target_info.name);
          set_target_asm(target_info.assemextern);
          Message1(option_asm_forced,target_asm.idtxt);
        end;
      if (af_no_debug in asminfos[paratargetasm]^.flags) and
         (paratargetdbg<>dbg_none) then
        begin
          Message1(option_confict_asm_debug,
            asminfos[paratargetasm]^.idtxt);
          paratargetdbg:=dbg_none;
          exclude(init_settings.moduleswitches,cs_debuginfo);
        end;
    end;

  { maybe override debug info format }
  if (paratargetdbg<>dbg_none) then
    set_target_dbg(paratargetdbg);

  { switch assembler if it's binary and we got -a on the cmdline }
  if (cs_asm_leave in init_settings.globalswitches) and
     (af_outputbinary in target_asm.flags) then
   begin
     Message(option_switch_bin_to_src_assembler);
     set_target_asm(target_info.assemextern);
   end;

  { Force use of external linker if there is no
    internal linker or the linking is skipped }
  if not(cs_link_extern in init_settings.globalswitches) and
     (not assigned(target_info.link) or
      (cs_link_nolink in init_settings.globalswitches)) then
    include(init_settings.globalswitches,cs_link_extern);

  { turn off stripping if compiling with debuginfo or profile }
  if (
      (cs_debuginfo in init_settings.moduleswitches) or
      (cs_profile in init_settings.moduleswitches)
     ) and
     not(cs_link_separate_dbg_file in init_settings.globalswitches) then
    exclude(init_settings.globalswitches,cs_link_strip);

  { force fpu emulation on arm/wince, arm/gba and arm/nds}
  if (target_info.system in [system_arm_wince,system_arm_gba,system_m68k_amiga,
    system_m68k_linux,system_arm_nds])
{$ifdef arm}
    or (init_settings.fputype=fpu_soft)
{$endif arm}
  then
    begin
{$ifdef cpufpemu}
      include(init_settings.moduleswitches,cs_fp_emulation);
      { cs_fp_emulation and fpu_soft are equal on arm }
      init_settings.fputype:=fpu_soft;
{$endif cpufpemu}
    end;


  { now we can defined cpu and cpu type }
  def_system_macro('CPU'+Cputypestr[init_settings.cputype]);

  def_system_macro('FPU'+fputypestr[init_settings.fputype]);

{$ifdef ARM}
  { define FPC_DOUBLE_HILO_SWAPPED if needed to properly handle doubles in RTL }
  if (init_settings.fputype in [fpu_fpa,fpu_fpa10,fpu_fpa11]) and
    not(cs_fp_emulation in init_settings.moduleswitches) then
    def_system_macro('FPC_DOUBLE_HILO_SWAPPED');
{$endif ARM}

  { Section smartlinking conflicts with import sections on Windows }
  if GenerateImportSection and
     (target_info.system in [system_i386_win32,system_x86_64_win64]) then
    exclude(target_info.flags,tf_smartlink_sections);

  if not LinkTypeSetExplicitly then
    set_default_link_type;

  { Default alignment settings,
    1. load the defaults for the target
    2. override with generic optimizer setting (little size)
    3. override with the user specified -Oa }
  UpdateAlignment(init_settings.alignment,target_info.alignment);
  if (cs_opt_size in current_settings.optimizerswitches) then
   begin
     init_settings.alignment.procalign:=1;
     init_settings.alignment.jumpalign:=1;
     init_settings.alignment.loopalign:=1;
   end;

  UpdateAlignment(init_settings.alignment,option.paraalignment);

  set_system_macro('FPC_VERSION',version_nr);
  set_system_macro('FPC_RELEASE',release_nr);
  set_system_macro('FPC_PATCH',patch_nr);

  for i:=low(tfeature) to high(tfeature) do
    if i in features then
      def_system_macro('FPC_HAS_FEATURE_'+featurestr[i]);
  option.free;
  Option:=nil;
end;


initialization
  coption:=toption;
finalization
  if assigned(option) then
   option.free;
end.
