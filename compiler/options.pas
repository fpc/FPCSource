{
    $Id$
    Copyright (c) 1993-98 by the FPC development team

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

interface

uses
  verbose;

type
  POption=^TOption;
  TOption=object
    FirstPass,
    NoPressEnter,
    DoWriteLogo : boolean;
    FileLevel : longint;
    ParaIncludePath,
    ParaUnitPath,
    ParaObjectPath,
    ParaLibraryPath : string;
    Constructor Init;
    Destructor Done;
    procedure WriteLogo;
    procedure WriteInfo;
    procedure WriteHelpPages;
    procedure QuickInfo(const s:string);
    procedure IllegalPara(const opt:string);
    function  Unsetbool(const opts:string; pos: Longint):boolean;
    procedure interpret_proc_specific_options(const opt:string);virtual;
    procedure interpret_option(const opt :string);
    procedure Interpret_file(const filename : string);
    procedure Read_Parameters;
    procedure parsecmd(cmd:string);
  end;

procedure read_arguments(cmd:string);


implementation

uses
{$ifdef Delphi}
  dmisc,
{$else Delphi}
  dos,
{$endif Delphi}
  globtype,version,systems,
  cobjects,globals,
  scanner,link,messages
{$ifdef BrowserLog}
  ,browlog
{$endif BrowserLog}
{$ifdef i386}
  ,opts386
{$endif}
{$ifdef m68k}
  ,opts68k
{$endif}
  ;

const
  page_size = 24;

var
  read_configfile,        { read config file, set when a cfgfile is found }
  target_is_set : boolean;  { do not allow contradictory target settings }
  asm_is_set  : boolean; { -T also change initoutputformat if not set idrectly }
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
  initdefines.concat(new(pstring_item,init(upper(s))));
end;


procedure undef_symbol(const s : string);
var
  item,next : pstring_item;
begin
  if s='' then
   exit;
  item:=pstring_item(initdefines.first);
  while assigned(item) do
   begin
     if (item^.str^=s) then
      begin
        next:=pstring_item(item^.next);
        initdefines.remove(item);
        dispose(item,done);
        item:=next;
      end
     else
      if item<>pstring_item(item^.next) then
       item:=pstring_item(item^.next)
      else
       break;
   end;
end;


function check_symbol(const s:string):boolean;
var
  hp : pstring_item;
begin
  hp:=pstring_item(initdefines.first);
  while assigned(hp) do
   begin
     if (hp^.str^=s) then
      begin
        check_symbol:=true;
        exit;
      end;
     hp:=pstring_item(hp^.next);
   end;
  check_symbol:=false;
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

procedure Toption.WriteLogo;
var
  i : tmsgconst;
begin
  MaybeLoadMessageFile;
  for i:=option_logo_start to option_logo_end do
   Message1(i,target_cpu_string);
end;


procedure Toption.WriteInfo;
var
  i : tmsgconst;
begin
  MaybeLoadMessageFile;
  for i:=option_info_start to option_info_end do
   Message(i);
  Stop;
end;


procedure Toption.WriteHelpPages;

  function PadEnd(s:string;i:longint):string;
  begin
    while (length(s)<i) do
     s:=s+' ';
    PadEnd:=s;
  end;

var
  idx,
  lastident,
  j,outline,
  ident,
  lines : longint;
  show  : boolean;
  opt   : string[32];
  input,
  s     : string;
begin
  MaybeLoadMessageFile;
  Message1(option_usage,paramstr(0));
  lastident:=0;
  if DoWriteLogo then
   lines:=3
  else
   lines:=1;
  for idx:=ord(ol_begin) to ord(ol_end) do
   begin
   { get a line and reset }
     s:=msg^.Get(idx);
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
{$ifdef TP}
         't',
{$endif}
{$ifdef GDB}
         'g',
{$endif}
{$ifdef linux}
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
               stop;
            end;
           lines:=0;
         end;
        Comment(V_Normal,PadEnd('',ident)+opt+Copy(s,j+1,255));
        LastIdent:=Ident;
        inc(Lines);
      end;
   end;
  stop;
end;


procedure Toption.QuickInfo(const s:string);
begin
  Writeln(s);
  Stop;
end;


procedure Toption.IllegalPara(const opt:string);
begin
  Message1(option_illegal_para,opt);
  Message(option_help_pages_para);
  stop;
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


procedure TOption.interpret_option(const opt:string);
var
  code : integer;
  c    : char;
  more : string;
  j,l  : longint;
  d    : DirStr;
  e    : ExtStr;
begin
  if opt='' then
   exit;
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
                          browserlog.elements_to_list^.insert(more);
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
                            'x' :
                               If UnsetBool(More, j) then
                                 Begin
                                   initmoduleswitches:=initmoduleswitches-[cs_smartlink];
                                   inc(j)
                                 End
                               Else
                                 initmoduleswitches:=initmoduleswitches+[cs_smartlink];
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
                       'D' : utilsdirectory:=FixPath(More,true);
                       'e' : SetRedirectFile(More);
                       'E' : OutputExeDir:=FixPath(More,true);
                       'i' : if firstpass then
                              AddPathToList(includesearchpath,More,false)
                             else
                              AddPathToList(ParaIncludePath,More,false);
                       'g' : Message2(option_obsolete_switch_use_new,'-Fg','-Fl');
                       'l' : if firstpass then
                              AddPathToList(LibrarySearchPath,More,false)
                             else
                              AddPathToList(ParaLibraryPath,More,false);
                       'L' : if More<>'' then
                              ParaDynamicLinker:=More
                             else
                              IllegalPara(opt);
                       'o' : if firstpass then
                              AddPathToList(objectsearchpath,More,false)
                             else
                              AddPathToList(ParaObjectPath,More,false);
                       'r' : Msgfilename:=More;
                       'u' : if firstpass then
                              AddPathToList(unitsearchpath,More,false)
                             else
                              AddPathToList(ParaUnitPath,More,false);
                       'U' : OutputUnitDir:=FixPath(More,true);
                      else
                        IllegalPara(opt);
                      end;
                    end;
              'g' : begin
                      if UnsetBool(More, 0) then
                        initmoduleswitches:=initmoduleswitches-[cs_debuginfo]
                      else
                       begin
{$ifdef GDB}
                         initmoduleswitches:=initmoduleswitches+[cs_debuginfo];
                         for j:=1 to length(more) do
                          case more[j] of
                           'd' : initglobalswitches:=initglobalswitches+[cs_gdb_dbx];
                           'g' : initglobalswitches:=initglobalswitches+[cs_gdb_gsym];
                           'h' : initglobalswitches:=initglobalswitches+[cs_gdb_heaptrc];
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
              'i' : if more='' then
                     WriteInfo
                    else
                     begin
                        { Specific info, which can be used in Makefiles }
                        case More[1] of
                          'S' : begin
                                  case More[2] of
                                   'O' : QuickInfo(source_os.shortname);
{$ifdef Delphi !!!!!!!!!}
                                   'P' : QuickInfo('unknown');
{$else}
                                   'P' : QuickInfo(source_cpu_string);
{$endif}
                                  end;
                                end;
                          'T' : begin
                                  case More[2] of
                                   'O' : QuickInfo(target_os.shortname);
                                   'P' : QuickInfo(target_cpu_string);
                                  end;
                                end;
                          'V' : QuickInfo(version_string);
                          'D' : QuickInfo(date_string);
                        else
                          IllegalPara(Opt);
                        end;
                     end;
              'I' : if firstpass then
                     AddPathToList(includesearchpath,More,false)
                    else
                     AddPathToList(ParaIncludePath,More,false);
              'k' : if more<>'' then
                     ParaLinkOptions:=ParaLinkOptions+' '+More
                    else
                     IllegalPara(opt);
              'l' : if more='' then
                      DoWriteLogo:=true
                    else
                      IllegalPara(opt);
              'n' : if More='' then
                     read_configfile:=false
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
{$ifdef linux}
              'P' : initglobalswitches:=initglobalswitches+[cs_asm_pipe];
{$endif}
              's' : initglobalswitches:=initglobalswitches+[cs_asm_extern,cs_link_extern];
              'S' : begin
                      for j:=1 to length(more) do
                       case more[j] of
                        '2' : initmodeswitches:=objfpcmodeswitches;
                        'c' : initmoduleswitches:=initmoduleswitches+[cs_support_c_operators];
                        'd' : initmodeswitches:=delphimodeswitches;
                        'e' : begin
                                val(copy(more,j+1,length(more)-j),l,code);
                                if (code<>0) then
                                 SetMaxErrorCount(1)
                                else
                                 begin
                                   SetMaxErrorCount(l);
                                   break;
                                 end;
                              end;
                        'g' : initmoduleswitches:=initmoduleswitches+[cs_support_goto];
                        'h' : initlocalswitches:=initlocalswitches+[cs_ansistrings];
                        'i' : initmoduleswitches:=initmoduleswitches+[cs_support_inline];
                        'm' : initmoduleswitches:=initmoduleswitches+[cs_support_macro];
                        'o':  initmodeswitches:=tpmodeswitches;
                        'p' : initmodeswitches:=gpcmodeswitches;
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
                                 RelocSection:=true;
                               break;
                             end;
                        'C': apptype:=at_cui;
                        'G': apptype:=at_gui;
                        'N': begin
                               RelocSection:=false;
                             end;

                        'R': RelocSection:=true;
                       else
                        IllegalPara(opt);
                       end;
                    end;
              'X' : begin
                      for j:=1 to length(More) do
                       case More[j] of
                        'c' : initglobalswitches:=initglobalswitches+[cs_link_toc];
                        's' : initglobalswitches:=initglobalswitches+[cs_link_strip];
                        'D' : begin
                                def_symbol('FPC_LINK_DYNAMIC');
                                undef_symbol('FPC_LINK_SMART');
                                undef_symbol('FPC_LINK_STATIC');
                                initglobalswitches:=initglobalswitches+[cs_link_shared];
                                initglobalswitches:=initglobalswitches-[cs_link_static,cs_link_smart];
                              end;
                        'S' : begin
                                def_symbol('FPC_LINK_STATIC');
                                undef_symbol('FPC_LINK_SMART');
                                undef_symbol('FPC_LINK_DYNAMIC');
                                initglobalswitches:=initglobalswitches+[cs_link_static];
                                initglobalswitches:=initglobalswitches-[cs_link_shared,cs_link_smart];
                              end;
                        'X' : begin
                                def_symbol('FPC_LINK_SMART');
                                undef_symbol('FPC_LINK_STATIC');
                                undef_symbol('FPC_LINK_DYNAMIC');
                                initglobalswitches:=initglobalswitches+[cs_link_smart];
                                initglobalswitches:=initglobalswitches-[cs_link_shared,cs_link_static];
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
         Stop;
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
begin
{ avoid infinite loop }
  Inc(FileLevel);
  If FileLevel>MaxLevel then
   Message(option_too_many_cfg_files);
{ open file }
  assign(f,filename);
{$ifdef extdebug}
  Comment(V_Info,'trying to open file: '+filename);
{$endif extdebug}
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
     if (opts<>'') then
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
                  stop;
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
                  stop;
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
                  stop;
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
           if (not skip[level]) and (opts[1]='-') then
            interpret_option(opts)
         end;
      end;
   end;
  if Level>0 then
   Message(option_too_less_endif);
  Close(f);
  Dec(FileLevel);
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
     opts:=paramstr(paramindex);
     if firstpass then
      begin
      { only parse define,undef,target,verbosity and link options }
        if (opts[1]='-') and (opts[2] in ['i','d','v','T','u','n','X']) then
         interpret_option(opts);
      end
     else
      begin
        if opts[1]='@' then
         begin
           Delete(opts,1,1);
           Message1(option_reading_further_from,opts);
           interpret_file(opts);
         end
        else
         interpret_option(opts);
      end;
   end;
end;


procedure toption.parsecmd(cmd:string);
var
  i    : longint;
  opts : string;
begin
  while (cmd<>'') do
   begin
     while cmd[1]=' ' do
      delete(cmd,1,1);
     i:=pos(' ',cmd);
     if i=0 then
      i:=255;
     opts:=Copy(cmd,1,i-1);
     Delete(cmd,1,i);
     if firstpass then
      begin
      { only parse define,undef,target,verbosity and link options }
        if (opts[1]='-') and (opts[2] in ['d','v','T','u','n','X']) then
         interpret_option(opts);
      end
     else
      begin
        if opts[1]='@' then
         begin
           Delete(opts,1,1);
           Message1(option_reading_further_from,opts);
           interpret_file(opts);
         end
        else
         interpret_option(opts);
      end;
   end;
end;


constructor TOption.Init;
begin
  DoWriteLogo:=false;
  NoPressEnter:=false;
  FirstPass:=false;
  FileLevel:=0;
  ParaIncludePath:='';
  ParaObjectPath:='';
  ParaUnitPath:='';
  ParaLibraryPath:='';
end;


destructor TOption.Done;
begin
end;


{****************************************************************************
                              Callable Routines
****************************************************************************}

procedure read_arguments(cmd:string);
var
  configpath : pathstr;
  option     : poption;
begin
{$ifdef Delphi}
  option:=new(poption386,Init);
{$endif Delphi}
{$ifdef i386}
  option:=new(poption386,Init);
{$endif}
{$ifdef m68k}
  option:=new(poption68k,Init);
{$endif}
{$ifdef alpha}
  option:=new(poption,Init);
{$endif}
{$ifdef powerpc}
  option:=new(poption,Init);
{$endif}
{ Load messages }
  if (cmd='') and (paramcount=0) then
   Option^.WriteHelpPages;

{ default defines }
  def_symbol(target_info.short_name);
  def_symbol('FPK');
  def_symbol('FPC');
  def_symbol('VER'+version_nr);
  def_symbol('VER'+version_nr+'_'+release_nr);
  def_symbol('VER'+version_nr+'_'+release_nr+'_'+patch_nr);
{$ifdef newcg}
  def_symbol('WITHNEWCG');
{$endif}
{ Temporary defines, until things settle down }
  def_symbol('INT64');
  def_symbol('HASRESOURCESTRINGS');
  def_symbol('HASSAVEREGISTERS');
  def_symbol('NEWVMTOFFSET');
  def_symbol('HASINTERNMATH');
{ some stuff for TP compatibility }
{$ifdef i386}
  def_symbol('CPU86');
  def_symbol('CPU87');
  if (target_info.target in [target_i386_GO32V1,target_i386_GO32V2]) then
   def_symbol('DPMI'); { MSDOS is not defined in BP when target is DPMI }
{$endif}
{$ifdef m68k}
  def_symbol('CPU68');
{$endif}
{$ifdef ALPHA}
  def_symbol('ALPHA');
{$endif}
{$ifdef powerpc}
  def_symbol('POWERPC');
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
{$ifdef m68k}
     ppccfg:='ppc.cfg';
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
{$ifdef linux}
  if configpath='' then
   configpath:='/etc/';
{$endif}
  if ppccfg<>'' then
   begin
     read_configfile:=true;
     if not FileExists(ppccfg) then
      begin
{$ifdef linux}
        if (dos.getenv('HOME')<>'') and FileExists(FixPath(dos.getenv('HOME'),false)+'.'+ppccfg) then
         ppccfg:=FixPath(dos.getenv('HOME'),false)+'.'+ppccfg
        else
{$endif}
         if FileExists(configpath+ppccfg) then
          ppccfg:=configpath+ppccfg
        else
{$ifndef linux}
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
   { read the parameters quick, only -v -T }
     option^.firstpass:=true;
     if cmd<>'' then
       option^.parsecmd(cmd)
     else
       option^.read_parameters;
     if read_configfile then
      begin
{$ifdef EXTDEBUG}
        Comment(V_Debug,'read config file: '+ppccfg);
{$endif EXTDEBUG}
        option^.interpret_file(ppccfg);
      end;
   end;
  option^.firstpass:=false;
  if cmd<>'' then
    option^.parsecmd(cmd)
  else
    option^.read_parameters;

{ Stop if errors in options }
  if ErrorCount>0 then
   Stop;

{ write logo if set }
  if option^.DoWriteLogo then
   option^.WriteLogo;

{ Check file to compile }
  if param_file='' then
   begin
     Message(option_no_source_found);
     Stop;
   end;
{$ifndef linux}
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
  AddPathToList(UnitSearchPath,Option^.ParaUnitPath,true);
  AddPathToList(ObjectSearchPath,Option^.ParaObjectPath,true);
  AddPathToList(IncludeSearchPath,Option^.ParaIncludePath,true);
  AddPathToList(LibrarySearchPath,Option^.ParaLibraryPath,true);

{ add unit environment and exepath to the unit search path }
  if inputdir<>'' then
   AddPathToList(Unitsearchpath,inputdir,true);
{$ifdef Delphi}
  AddPathToList(UnitSearchPath,dmisc.getenv(target_info.unit_env),false);
{$else}
  AddPathToList(UnitSearchPath,dos.getenv(target_info.unit_env),false);
{$endif Delphi}
  AddPathToList(UnitSearchPath,ExePath,false);
  { Add unit dir to the object and library path }
  AddPathToList(objectsearchpath,unitsearchpath,false);
  AddPathToList(librarysearchpath,unitsearchpath,false);

{ switch assembler if it's binary and we got -a on the cmdline }
  if (cs_asm_leave in initglobalswitches) and
     (target_asm.id in binassem) then
   begin
     Message(option_switch_bin_to_src_assembler);
     set_target_asm(target_info.assemsrc);
     initoutputformat:=target_asm.id;
   end;

{ turn off stripping if compiling with debuginfo or profile }
  if (cs_debuginfo in initmoduleswitches) or
     (cs_profile in initmoduleswitches) then
    initglobalswitches:=initglobalswitches-[cs_link_strip];

  MaybeLoadMessageFile;

  dispose(option,Done);
end;


end.
{
  $Log$
  Revision 1.21  1999-09-15 20:35:40  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.20  1999/09/03 09:31:22  peter
    * reading of search paths fixed to work as expected

  Revision 1.19  1999/09/01 22:07:20  peter
    * turn off stripping if profiling or debugging

  Revision 1.18  1999/08/28 17:46:10  peter
    * resources are working correct

  Revision 1.17  1999/08/28 15:34:19  florian
    * bug 519 fixed

  Revision 1.16  1999/08/27 10:45:03  pierre
   options -Ca sets simply_ppu to true

  Revision 1.15  1999/08/25 22:51:00  pierre
   * remove trailing space in cfg files

  Revision 1.14  1999/08/16 15:35:26  pierre
    * fix for DLL relocation problems
    * external bss vars had wrong stabs for pecoff
    + -WB11000000 to specify default image base, allows to
      load several DLLs with debugging info included
      (relocatable DLL are stripped because the relocation
       of the .Stab section is misplaced by ldw)

  Revision 1.13  1999/08/11 17:26:35  peter
    * tlinker object is now inherited for win32 and dos
    * postprocessexecutable is now a method of tlinker

  Revision 1.12  1999/08/10 12:51:17  pierre
    * bind_win32_dll removed (Relocsection used instead)
    * now relocsection is true by default ! (needs dlltool
      for DLL generation)

  Revision 1.11  1999/08/09 22:19:52  peter
    * classes vmt changed to only positive addresses
    * sharedlib creation is working

  Revision 1.10  1999/08/05 23:45:10  peter
    * saveregister is now working and used for assert and iocheck (which has
      been moved to system.inc because it's now system independent)

  Revision 1.9  1999/08/04 13:02:46  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.8  1999/08/03 17:09:36  florian
    * the alpha compiler can be compiled now

  Revision 1.7  1999/08/02 23:13:19  florian
    * more changes to compile for the Alpha

  Revision 1.6  1999/07/23 22:56:27  michael
  + Added HasResourceStrings define

  Revision 1.5  1999/07/18 10:19:57  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.4  1999/07/05 20:13:14  peter
    * removed temp defines

  Revision 1.3  1999/07/03 00:29:54  peter
    * new link writing to the ppu, one .ppu is needed for all link types,
      static (.o) is now always created also when smartlinking is used

  Revision 1.2  1999/07/01 15:49:19  florian
    * int64/qword type release
    + lo/hi for int64/qword

  Revision 1.1  1999/06/11 13:28:40  peter
    * reinserted

  Revision 1.111  1999/06/10 23:52:31  pierre
   * merged from fixes branch

  Revision 1.110.2.1  1999/06/10 23:37:17  pierre
   * language switch before help screen

  Revision 1.110  1999/05/27 19:44:40  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.109  1999/05/18 14:11:46  peter
    * stop after errors in options

  Revision 1.108  1999/05/17 14:14:15  pierre
   + -gc for check pointer with heaptrc

  Revision 1.107  1999/05/16 02:28:59  peter
    * removed -Fg and -Up

  Revision 1.106  1999/05/06 09:05:20  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.105  1999/05/04 21:44:51  florian
    * changes to compile it with Delphi 4.0

  Revision 1.104  1999/05/02 22:41:56  peter
    * moved section names to systems
    * fixed nasm,intel writer

  Revision 1.103  1999/05/01 13:24:25  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.102  1999/04/26 18:29:55  peter
    * farpointerdef moved into pointerdef.is_far

  Revision 1.101  1999/04/26 13:31:35  peter
    * release storenumber,double_checksum

  Revision 1.100  1999/04/17 13:12:21  peter
    * addr() internal

  Revision 1.99  1999/04/16 11:49:44  peter
    + tempalloc
    + -at to show temp alloc info in .s file

  Revision 1.98  1999/04/15 12:19:56  peter
    + finalization support

  Revision 1.97  1999/04/10 16:15:02  peter
    * fixed browcol
    + -ar to show regalloc info in .s file

  Revision 1.96  1999/04/08 15:57:49  peter
    + subrange checking for readln()

  Revision 1.95  1999/04/01 22:07:52  peter
    * universal string names (ansistr instead of stransi) for val/str

  Revision 1.94  1999/03/26 00:05:32  peter
    * released valintern
    + deffile is now removed when compiling is finished
    * ^( compiles now correct
    + static directive
    * shrd fixed

  Revision 1.93  1999/03/25 16:55:32  peter
    + unitpath,librarypath,includepath,objectpath directives

  Revision 1.92  1999/03/24 23:17:08  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.91  1999/03/16 17:52:51  jonas
    * changes for internal Val code (do a "make cycle OPT=-dvalintern" to test)
    * in cgi386inl: also range checking for subrange types (compile with "-dreadrangecheck")
    * in cgai386: also small fixes to emitrangecheck

  Revision 1.90  1999/03/11 13:39:15  pierre
   * initoutputformat also set by -T arg

  Revision 1.89  1999/03/02 02:56:13  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.88  1999/02/22 13:06:57  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.87  1999/02/03 10:18:12  pierre
   * conditionnal code for extended check of virtual methods

  Revision 1.86  1999/01/12 14:25:28  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.85  1998/12/30 22:04:34  michael
  + Added -Sh switch

  Revision 1.84  1998/12/28 15:45:48  peter
    * fixes for parse_cmd

  Revision 1.83  1998/12/19 00:23:50  florian
    * ansistring memory leaks fixed

  Revision 1.82  1998/12/18 17:25:50  peter
    * removed temp symbols again :)

  Revision 1.81  1998/12/16 14:29:00  jonas
    * use of UnsetBool instead of manually checking for -Option-

  Revision 1.80  1998/12/16 13:36:05  jonas
    * allow '-' after the -C? options to disable them

  Revision 1.78  1998/12/15 10:23:25  peter
    + -iSO, -iSP, -iTO, -iTP

  Revision 1.77  1998/12/14 17:23:05  peter
    + -iV, -iD

  Revision 1.76  1998/12/11 00:03:21  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.75  1998/12/08 10:18:10  peter
    + -gh for heaptrc unit

  Revision 1.74  1998/11/30 13:26:24  pierre
    * the code for ordering the exported procs/vars was buggy
    + added -WB to force binding (Ozerski way of creating DLL)
      this is off by default as direct writing of .edata section seems
      OK

  Revision 1.73  1998/11/30 09:43:18  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.72  1998/11/28 14:09:45  peter
    * NOATTCDQ define

  Revision 1.71  1998/11/27 14:50:39  peter
    + open strings, $P switch support

  Revision 1.70  1998/11/18 09:18:02  pierre
    + automatic loading of profile unit with -pg option
      in go32v2 mode (also defines FPC_PROFILE)
    * some memory leaks removed
    * unreleased temp problem with sets solved

  Revision 1.69  1998/11/17 11:32:45  peter
    * optimize str:='' in H+ mode
    + -! to test ansistrings

  Revision 1.68  1998/11/17 00:36:46  peter
    * more ansistring fixes

  Revision 1.67  1998/11/13 15:40:20  pierre
    + added -Se in Makefile cvstest target
    + lexlevel cleanup
      normal_function_level main_program_level and unit_init_level defined
    * tins_cache grown to A_EMMS (gave range check error in asm readers)
      (test added in code !)
    * -Un option was wrong
    * _FAIL and _SELF only keyword inside
      constructors and methods respectively

  Revision 1.66  1998/11/13 10:18:08  peter
    + nil constants

  Revision 1.65  1998/11/11 12:34:53  peter
    * fixed small typo

  Revision 1.64  1998/11/10 17:54:12  peter
    * -g disables linker.strip

  Revision 1.63  1998/11/05 12:02:49  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.62  1998/11/04 10:11:39  peter
    * ansistring fixes

  Revision 1.61  1998/10/27 08:24:11  pierre
   -lS and -lT options changed to -iS and -iT

  Revision 1.60  1998/10/26 22:23:33  peter
    + fixpath() has an extra option to allow a ./ as path

  Revision 1.59  1998/10/26 14:19:27  pierre
    + added options -lS and -lT for source and target os output
      (to have a easier way to test OS_SOURCE abd OS_TARGET in makefiles)
    * several problems with rtti data
      (type of sym was not checked)
      assumed to be varsym when they could be procsym or property syms !!

  Revision 1.58  1998/10/20 09:32:55  peter
    * removed some unused vars

  Revision 1.57  1998/10/16 13:37:23  florian
    + switch -FD added to specify the path for utilities

  Revision 1.56  1998/10/15 12:37:43  pierre
    + passes vmt offset to HELP_CONSTRUCTOR for objects

  Revision 1.55  1998/10/13 13:10:19  peter
    * new style for m68k/i386 infos and enums

  Revision 1.54  1998/10/13 08:19:40  pierre
    + source_os is now set correctly for cross-processor compilers
      (tos contains all target_infos and
       we use CPU86 and CPU68 conditionnals to
       get the source operating system
       this only works if you do not undefine
       the source target  !!)
    * several cg68k memory leaks fixed
    + started to change the code so that it should be possible to have
      a complete compiler (both for m68k and i386 !!)

  Revision 1.53  1998/10/09 13:00:25  pierre
    * msgfile existence tested

  Revision 1.52  1998/10/08 17:17:22  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.51  1998/10/05 13:53:42  peter
    + -Se<value>

  Revision 1.50  1998/10/02 09:24:21  peter
    * more constant expression evaluators

  Revision 1.49  1998/09/26 17:45:29  peter
    + idtoken and only one token table

  Revision 1.48  1998/09/25 09:57:07  peter
    * moved -A to options.pas, becuase the code is the same

  Revision 1.47  1998/09/24 23:47:03  peter
    + -FE,-FU,-Sd

  Revision 1.46  1998/09/23 15:39:06  pierre
    * browser bugfixes
      was adding a reference when looking for the symbol
      if -bSYM_NAME was used

  Revision 1.45  1998/09/22 17:13:47  pierre
    + browsing updated and developed
      records and objects fields are also stored

  Revision 1.44  1998/09/21 08:45:11  pierre
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

  Revision 1.43  1998/09/18 08:01:33  pierre
    + improvement on the usebrowser part
      (does not work correctly for now)

  Revision 1.42  1998/09/14 10:44:09  peter
    * all internal RTL functions start with FPC_

  Revision 1.41  1998/09/03 11:21:51  peter
    * -al sets cs_asm_source

  Revision 1.40  1998/08/31 08:50:32  peter
    * fixed default msgfile loading which is now in verbose.pas

  Revision 1.39  1998/08/29 13:52:38  peter
    + new messagefile
    * merged optione.msg into errore.msg

  Revision 1.38  1998/08/25 12:42:38  pierre
    * CDECL changed to CVAR for variables
      specifications are read in structures also
    + started adding GPC compatibility mode ( option  -Sp)
    * names changed to lowercase

  Revision 1.37  1998/08/20 16:10:52  pierre
    Q
    * Changed the -E switch to get better use of RHIDE v1.4.5

  Revision 1.36  1998/08/18 09:05:59  peter
    * new library options
    * smartlink is now -Cx

  Revision 1.35  1998/08/17 09:17:48  peter
    * static/shared linking updates

  Revision 1.34  1998/08/14 21:56:35  peter
    * setting the outputfile using -o works now to create static libs

  Revision 1.33  1998/08/10 23:53:47  peter
    * released all temporary defines

  Revision 1.32  1998/08/10 14:50:03  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.30  1998/08/10 08:33:17  michael
  -d was destroyed by last comit

  Revision 1.29  1998/08/08 15:31:04  michael
  + Reinstated -e option for linux

  Revision 1.28  1998/07/17 10:06:18  michael
  + under linux, looking for ppc38.cfg in bindir removed.

  Revision 1.27  1998/07/07 11:19:58  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.26  1998/07/06 15:51:17  michael
  Added length checking for string reading

  Revision 1.25  1998/07/04 10:00:22  peter
    + define HAS_PROPERTY

  Revision 1.24  1998/07/01 15:28:49  peter
    + better writeln/readln handling, now 100% like tp7

  Revision 1.22  1998/06/13 00:10:07  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.21  1998/06/12 16:15:32  pierre
    * external name 'C_var';
      export name 'intern_C_var';
      cdecl;
      cdecl;external;
      are now supported only with -Sv switch

  Revision 1.20  1998/06/08 22:59:47  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.19  1998/06/04 23:51:46  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.17  1998/05/25 17:11:40  pierre
    * firstpasscount bug fixed
      now all is already set correctly the first time
      under EXTDEBUG try -gp to skip all other firstpasses
      it works !!
    * small bug fixes
      - for smallsets with -dTESTSMALLSET
      - some warnings removed (by correcting code !)

  Revision 1.16  1998/05/23 01:21:12  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.15  1998/05/20 09:42:34  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.14  1998/05/12 10:46:59  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.13  1998/05/08 09:21:20  michael
  * Added missing -Fl message to messages file.
  * Corrected mangling of file names when doing Linklib
  * -Fl now actually WORKS.
  * Librarysearchpath is now a field in linker object.

  Revision 1.12  1998/05/06 08:38:41  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.11  1998/05/04 17:54:27  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.10  1998/05/01 16:38:44  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.9  1998/04/30 15:59:40  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.8  1998/04/27 15:45:20  peter
    + -Xl for smartlink
    + target_info.arext = .a

  Revision 1.7  1998/04/23 12:07:25  peter
    * fixed -i

  Revision 1.6  1998/04/08 16:58:03  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.5  1998/04/08 12:31:00  peter
    + .ppc386.cfg and #INCLUDE support

  Revision 1.4  1998/04/07 13:19:46  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)
}
