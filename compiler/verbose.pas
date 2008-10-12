{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit handles the verbose management

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
unit verbose;

{$i fpcdefs.inc}

interface

    uses
{$IFNDEF USE_FAKE_SYSUTILS}
      sysutils,
{$ELSE}
      fksysutl,
{$ENDIF}
      cutils,
      globtype,finput,
      cmsgs;

{$ifndef EXTERN_MSG}
  {$i msgtxt.inc}
{$endif}

{$i msgidx.inc}

    Const
      { Levels }
      V_None         = $0;
      V_Fatal        = $1;
      V_Error        = $2;
      V_Normal       = $4; { doesn't show a text like Error: }
      V_Warning      = $8;
      V_Note         = $10;
      V_Hint         = $20;
      V_LineInfoMask = $fff;
      { From here by default no line info }
      V_Info         = $1000;
      V_Status       = $2000;
      V_Used         = $4000;
      V_Tried        = $8000;
      V_Conditional  = $10000;
      V_Debug        = $20000;
      V_Executable   = $40000;
      V_LevelMask    = $fffffff;
      V_All          = V_LevelMask;
      V_Default      = V_Fatal + V_Error + V_Normal;
      { Flags }
      V_LineInfo     = $10000000;

    var
      msg : pmessage;
      paraprintnodetree : byte;

    type
      tmsgqueueevent = procedure(s:string;v,w:longint) of object;

    const
      msgfilename : string = '';

    procedure SetRedirectFile(const fn:string);
    function  SetVerbosity(const s:string):boolean;
    procedure PrepareReport;

    function  CheckVerbosity(v:longint):boolean;
    procedure ShowStatus;
    function  ErrorCount:longint;
    procedure SetErrorFlags(const s:string);
    procedure GenerateError;
    procedure Internalerror(i:longint);
    procedure Comment(l:longint;s:ansistring);
    function  MessagePchar(w:longint):pchar;
    procedure Message(w:longint;onqueue:tmsgqueueevent=nil);
    procedure Message1(w:longint;const s1:string;onqueue:tmsgqueueevent=nil);
    procedure Message2(w:longint;const s1,s2:string;onqueue:tmsgqueueevent=nil);
    procedure Message3(w:longint;const s1,s2,s3:string;onqueue:tmsgqueueevent=nil);
    procedure Message4(w:longint;const s1,s2,s3,s4:string;onqueue:tmsgqueueevent=nil);
    procedure MessagePos(const pos:tfileposinfo;w:longint;onqueue:tmsgqueueevent=nil);
    procedure MessagePos1(const pos:tfileposinfo;w:longint;const s1:string;onqueue:tmsgqueueevent=nil);
    procedure MessagePos2(const pos:tfileposinfo;w:longint;const s1,s2:string;onqueue:tmsgqueueevent=nil);
    procedure MessagePos3(const pos:tfileposinfo;w:longint;const s1,s2,s3:string;onqueue:tmsgqueueevent=nil);
    procedure MessagePos4(const pos:tfileposinfo;w:longint;const s1,s2,s3,s4:string;onqueue:tmsgqueueevent=nil);

    { message calls with codegenerror support }
    procedure cgmessage(t : longint);
    procedure cgmessage1(t : longint;const s : string);
    procedure cgmessage2(t : longint;const s1,s2 : string);
    procedure cgmessage3(t : longint;const s1,s2,s3 : string);
    procedure CGMessagePos(const pos:tfileposinfo;t:longint);
    procedure CGMessagePos1(const pos:tfileposinfo;t:longint;const s1:string);
    procedure CGMessagePos2(const pos:tfileposinfo;t:longint;const s1,s2:string);
    procedure CGMessagePos3(const pos:tfileposinfo;t:longint;const s1,s2,s3:string);

    procedure FlushOutput;

    procedure InitVerbose;
    procedure DoneVerbose;



implementation

    uses
      comphook,fmodule,constexp,globals;

{****************************************************************************
                       Extra Handlers for default compiler
****************************************************************************}

    procedure DoneRedirectFile;
      begin
        if status.use_redir then
         begin
           close(status.redirfile);
           status.use_redir:=false;
         end;
        if status.use_bugreport then
         begin
           close(status.reportbugfile);
           status.use_bugreport:=false;
         end;
      end;


    procedure SetRedirectFile(const fn:string);
      begin
        { close old redirection file because FileRedirection is handled in both passes }
        if status.use_redir then
          close(status.redirfile);

        assign(status.redirfile,fn);
      {$I-}
        append(status.redirfile);
        if ioresult <> 0 then
          begin
            assign(status.redirfile,fn);
            rewrite(status.redirfile);
          end;
      {$I+}
        status.use_redir:=(ioresult=0);
      end;


    procedure PrepareReport;
      var
        fn : string;
      begin
        if status.use_bugreport then
         exit;
        fn:='fpcdebug.txt';
        assign(status.reportbugfile,fn);
        {$I-}
         append(status.reportbugfile);
         if ioresult <> 0 then
          rewrite(status.reportbugfile);
        {$I+}
        status.use_bugreport:=(ioresult=0);
        if status.use_bugreport then
         writeln(status.reportbugfile,'FPC bug report file');
      end;


    function ClearMessageVerbosity(s: string; var i: integer): boolean;
      var
        tok : string;
        code : longint;
        msgnr: longint;
      begin
        { delete everything up to and including 'm' }
        delete(s,1,i);
        { the rest of the string must be message numbers }
        inc(i,length(s)+1);
        result:=false;
        repeat
          tok:=GetToken(s,',');
          if (tok='') then
            break;
          val(tok, msgnr, code);
          if (code<>0) then
            exit;
          if not msg^.clearverbosity(msgnr) then
            exit;
        until false;
        result:=true;
      end;


    function CheckVerbosity(v:longint):boolean;
      begin
        result:=do_checkverbosity(v);
      end;


    function SetVerbosity(const s:string):boolean;
      var
        m : Longint;
        i : Integer;
        inverse : boolean;
        c : char;
      begin
        Setverbosity:=false;
        val(s,m,i);
        if (i=0) and (s<>'') then
         status.verbosity:=m
        else
         begin
           i:=1;
           while i<=length(s) do
             begin
                c:=upcase(s[i]);
                inverse:=false;
                { on/off ? }
                if (i<length(s)) then
                 case s[i+1] of
                  '-' : begin
                          inc(i);
                          inverse:=true;
                        end;
                  '+' : inc(i);
                 end;
                { handle switch }
                case c of
                { Special cases }
                 '0' : status.verbosity:=V_Default;
                 'A' : status.verbosity:=V_All;
                 'B' : begin
                          if inverse then
                            status.print_source_path:=false
                          else
                            status.print_source_path:=true;
                       end;
                 'M' : if inverse or
                          not ClearMessageVerbosity(s, i) then
                         begin
                           result:=false;
                           exit
                         end;
                 'P' : begin
                         if inverse then
                          paraprintnodetree:=0
                         else
                          paraprintnodetree:=1;
                       end;
                 'Q' : begin
                          if inverse then
                            status.showmsgnrs:=false
                          else
                            status.showmsgnrs:=true;
                       end;
                 'R' : begin
                          if inverse then
                            begin
                               status.use_gccoutput:=false;
                               status.use_stderr:=false;
                            end
                          else
                            begin
                               status.use_gccoutput:=true;
                               status.use_stderr:=true;
                            end;
                       end;
                 'V' : PrepareReport;
                 'Z' : begin
                          if inverse then
                            status.use_stderr:=false
                          else
                            status.use_stderr:=true;
                       end;
                { Normal cases - do an or }
                 'C' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Conditional)
                       else
                         status.verbosity:=status.verbosity or V_Conditional;
                 'D' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Debug)
                       else
                         status.verbosity:=status.verbosity or V_Debug;
                 'E' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Error)
                       else
                         status.verbosity:=status.verbosity or V_Error;
                 'H' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Hint)
                       else
                         status.verbosity:=status.verbosity or V_Hint;
                 'I' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Info)
                       else
                         status.verbosity:=status.verbosity or V_Info;
                 'L' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Status)
                       else
                         status.verbosity:=status.verbosity or V_Status;
                 'N' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Note)
                       else
                         status.verbosity:=status.verbosity or V_Note;
                 'S' : if inverse then
                         status.verbosity:=status.verbosity and (not V_TimeStamps)
                       else
                         status.verbosity:=status.verbosity or V_TimeStamps;
                 'T' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Tried)
                       else
                         status.verbosity:=status.verbosity or V_Tried;
                 'U' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Used)
                       else
                         status.verbosity:=status.verbosity or V_Used;
                 'W' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Warning)
                       else
                         status.verbosity:=status.verbosity or V_Warning;
                 'X' : if inverse then
                         status.verbosity:=status.verbosity and (not V_Executable)
                       else
                         status.verbosity:=status.verbosity or V_Executable;
                 end;
                inc(i);
             end;
           end;
        if status.verbosity=0 then
         status.verbosity:=V_Default;
        setverbosity:=true;
      end;


    procedure Loadprefixes;

        function loadprefix(w:longint):string;
        var
          s : string;
          idx : longint;
        begin
          s:=msg^.get(w,[]);
          idx:=pos('_',s);
          if idx>0 then
           Loadprefix:=Copy(s,idx+1,255)
          else
           Loadprefix:=s;
        end;

      begin
      { Load the prefixes }
        fatalstr:=Loadprefix(general_i_fatal);
        errorstr:=Loadprefix(general_i_error);
        warningstr:=Loadprefix(general_i_warning);
        notestr:=Loadprefix(general_i_note);
        hintstr:=Loadprefix(general_i_hint);
      end;


    procedure LoadMsgFile(const fn:string);
      begin
        { reload the internal messages if not already loaded }
{$ifndef EXTERN_MSG}
        if not msg^.msgintern then
         msg^.LoadIntern(@msgtxt,msgtxtsize);
{$endif}
        if not msg^.LoadExtern(fn) then
         begin
{$ifdef EXTERN_MSG}
           writeln('Fatal: Cannot find error message file.');
           halt(3);
{$else}
           msg^.LoadIntern(@msgtxt,msgtxtsize);
{$endif}
         end;
        { reload the prefixes using the new messages }
        Loadprefixes;
      end;


    procedure MaybeLoadMessageFile;
      begin
        { Load new message file }
        if (msgfilename<>'')  then
         begin
           LoadMsgFile(msgfilename);
           msgfilename:='';
         end;
      end;


    var
      lastfileidx,
      lastmoduleidx : longint;


    Procedure UpdateStatus;
      var
        module : tmodulebase;
      begin
      { fix status }
        status.currentline:=current_filepos.line;
        status.currentcolumn:=current_filepos.column;
        if (current_filepos.moduleindex <> lastmoduleidx) or
           (current_filepos.fileindex <> lastfileidx) then
        begin
          module:=get_module(current_filepos.moduleindex);
          if assigned(module) and assigned(module.sourcefiles) then
            begin
              { update status record }
              status.currentmodule:=module.modulename^;
              status.currentmodulestate:=ModuleStateStr[module.state];
              status.currentsource:=module.sourcefiles.get_file_name(current_filepos.fileindex);
              status.currentsourcepath:=module.sourcefiles.get_file_path(current_filepos.fileindex);

              { update lastfileidx only if name known PM }
              if status.currentsource<>'' then
                lastfileidx:=current_filepos.fileindex
              else
                lastfileidx:=0;

              lastmoduleidx:=module.unit_index;
            end;
        end;
      end;


    procedure ShowStatus;
      begin
        UpdateStatus;
        if do_status() then
          raise ECompilerAbort.Create;
      end;


    function ErrorCount:longint;
      begin
        ErrorCount:=status.errorcount;
      end;


    procedure SetErrorFlags(const s:string);
      var
        code : integer;
        i,j,l : longint;
      begin
      { empty string means error count = 1 for backward compatibility (PFV) }
        if s='' then
         begin
           status.maxerrorcount:=1;
           exit;
         end;
        i:=0;
        while (i<length(s)) do
         begin
           inc(i);
           case s[i] of
             '0'..'9' :
                begin
                  j:=i;
                  while (j<=length(s)) and (s[j] in ['0'..'9']) do
                   inc(j);
                  val(copy(s,i,j-i),l,code);
                  if code<>0 then
                   l:=1;
                  status.maxerrorcount:=l;
                  i:=j-1;
                end;
              'w','W' :
                status.errorwarning:=true;
              'n','N' :
                status.errornote:=true;
              'h','H' :
                status.errorhint:=true;
           end;
         end;
      end;


    procedure GenerateError;
      begin
        inc(status.errorcount);
      end;


    procedure internalerror(i : longint);
      begin
        UpdateStatus;
        do_internalerror(i);
        inc(status.errorcount);
        raise ECompilerAbort.Create;
      end;


    procedure Comment(l:longint;s:ansistring);
      var
        dostop : boolean;
      begin
        dostop:=((l and V_Fatal)<>0);
        if ((l and V_Error)<>0) or
           ((l and V_Fatal)<>0) or
           (status.errorwarning and ((l and V_Warning)<>0)) or
           (status.errornote and ((l and V_Note)<>0)) or
           (status.errorhint and ((l and V_Hint)<>0)) then
         inc(status.errorcount)
        else
         if l and V_Warning <> 0 then
          inc(status.countWarnings)
         else
          if l and V_Note <> 0 then
           inc(status.countNotes)
          else
           if l and V_Hint <> 0 then
            inc(status.countHints);
      { check verbosity level }
        if not CheckVerbosity(l) then
          exit;
        if (l and V_LineInfoMask)<>0 then
          l:=l or V_LineInfo;
      { Create status info }
        UpdateStatus;
      { Fix replacements }
        DefaultReplacements(s);
      { show comment }
        if do_comment(l,s) or dostop then
          raise ECompilerAbort.Create;
        if (status.errorcount>=status.maxerrorcount) and not status.skip_error then
         begin
           Message1(unit_f_errors_in_unit,tostr(status.errorcount));
           status.skip_error:=true;
           raise ECompilerAbort.Create;
         end;
      end;


    Procedure Msg2Comment(s:ansistring;w:longint;onqueue:tmsgqueueevent);
      var
        idx,i,v : longint;
        dostop  : boolean;
        doqueue : boolean;
      begin
      {Reset}
        dostop:=false;
        doqueue:=false;
        v:=0;
      {Parse options}
        idx:=pos('_',s);
        if idx=0 then
         v:=V_None
        else
         if (idx >= 1) And (idx <= 5) then
          begin
            for i:=1 to idx do
             begin
               case upcase(s[i]) of
                'F' :
                  begin
                    v:=v or V_Fatal;
                    inc(status.errorcount);
                    dostop:=true;
                  end;
                'E' :
                  begin
                    v:=v or V_Error;
                    inc(status.errorcount);
                  end;
                'O' :
                  v:=v or V_Normal;
                'W':
                  begin
                    v:=v or V_Warning;
                    if status.errorwarning then
                     inc(status.errorcount)
                    else
                     inc(status.countWarnings);
                  end;
                'N' :
                  begin
                    v:=v or V_Note;
                    if status.errornote then
                     inc(status.errorcount)
                    else
                     inc(status.countNotes);
                  end;
                'H' :
                  begin
                    v:=v or V_Hint;
                    if status.errorhint then
                     inc(status.errorcount)
                    else
                     inc(status.countHints);
                  end;
                'I' :
                  v:=v or V_Info;
                'L' :
                  v:=v or V_LineInfo;
                'U' :
                  v:=v or V_Used;
                'T' :
                  v:=v or V_Tried;
                'C' :
                  v:=v or V_Conditional;
                'D' :
                  v:=v or V_Debug;
                'X' :
                  v:=v or V_Executable;
                'S' :
                  dostop:=true;
                '_' : ;
               end;
             end;
          end;
        Delete(s,1,idx);
      { check verbosity level }
        if not CheckVerbosity(v) then
          begin
            doqueue := onqueue <> nil;
            if not doqueue then
              exit;
          end;
        if (v and V_LineInfoMask)<>0 then
          v:=v or V_LineInfo;
      { fix status }
        UpdateStatus;
      { Fix replacements }
        DefaultReplacements(s);
        if status.showmsgnrs then
          s:='('+tostr(w)+') '+s;
        if doqueue then
          begin
            onqueue(s,v,w);
            exit;
          end;
      { show comment }
        if do_comment(v,s) or dostop then
          raise ECompilerAbort.Create;
        if (status.errorcount>=status.maxerrorcount) and not status.skip_error then
          begin
            Message1(unit_f_errors_in_unit,tostr(status.errorcount));
            status.skip_error:=true;
            raise ECompilerAbort.Create;
          end;
      end;


    function  MessagePchar(w:longint):pchar;
      begin
        MaybeLoadMessageFile;
        MessagePchar:=msg^.GetPchar(w)
      end;


    procedure Message(w:longint;onqueue:tmsgqueueevent=nil);
      begin
        MaybeLoadMessageFile;
        Msg2Comment(msg^.Get(w,[]),w,onqueue);
      end;


    procedure Message1(w:longint;const s1:string;onqueue:tmsgqueueevent=nil);

      begin
        MaybeLoadMessageFile;
        Msg2Comment(msg^.Get(w,[s1]),w,onqueue);
      end;


    procedure Message2(w:longint;const s1,s2:string;onqueue:tmsgqueueevent=nil);
      begin
        MaybeLoadMessageFile;
        Msg2Comment(msg^.Get(w,[s1,s2]),w,onqueue);
      end;


    procedure Message3(w:longint;const s1,s2,s3:string;onqueue:tmsgqueueevent=nil);
      begin
        MaybeLoadMessageFile;
        Msg2Comment(msg^.Get(w,[s1,s2,s3]),w,onqueue);
      end;


    procedure Message4(w:longint;const s1,s2,s3,s4:string;onqueue:tmsgqueueevent=nil);
      begin
        MaybeLoadMessageFile;
        Msg2Comment(msg^.Get(w,[s1,s2,s3,s4]),w,onqueue);
      end;


    procedure MessagePos(const pos:tfileposinfo;w:longint;onqueue:tmsgqueueevent=nil);
      var
        oldpos : tfileposinfo;
      begin
        oldpos:=current_filepos;
        current_filepos:=pos;
        MaybeLoadMessageFile;
        Msg2Comment(msg^.Get(w,[]),w,onqueue);
        current_filepos:=oldpos;
      end;


    procedure MessagePos1(const pos:tfileposinfo;w:longint;const s1:string;onqueue:tmsgqueueevent=nil);
      var
        oldpos : tfileposinfo;
      begin
        oldpos:=current_filepos;
        current_filepos:=pos;
        MaybeLoadMessageFile;
        Msg2Comment(msg^.Get(w,[s1]),w,onqueue);
        current_filepos:=oldpos;
      end;


    procedure MessagePos2(const pos:tfileposinfo;w:longint;const s1,s2:string;onqueue:tmsgqueueevent=nil);
      var
        oldpos : tfileposinfo;
      begin
        oldpos:=current_filepos;
        current_filepos:=pos;
        MaybeLoadMessageFile;
        Msg2Comment(msg^.Get(w,[s1,s2]),w,onqueue);
        current_filepos:=oldpos;
      end;


    procedure MessagePos3(const pos:tfileposinfo;w:longint;const s1,s2,s3:string;onqueue:tmsgqueueevent=nil);
      var
        oldpos : tfileposinfo;
      begin
        oldpos:=current_filepos;
        current_filepos:=pos;
        MaybeLoadMessageFile;
        Msg2Comment(msg^.Get(w,[s1,s2,s3]),w,onqueue);
        current_filepos:=oldpos;
      end;


    procedure MessagePos4(const pos:tfileposinfo;w:longint;const s1,s2,s3,s4:string;onqueue:tmsgqueueevent=nil);
      var
        oldpos : tfileposinfo;
      begin
        oldpos:=current_filepos;
        current_filepos:=pos;
        MaybeLoadMessageFile;
        Msg2Comment(msg^.Get(w,[s1,s2,s3,s4]),w,onqueue);
        current_filepos:=oldpos;
      end;


{*****************************************************************************
            override the message calls to set codegenerror
*****************************************************************************}

    procedure cgmessage(t : longint);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.Message(t);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessage1(t : longint;const s : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.Message1(t,s);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessage2(t : longint;const s1,s2 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.Message2(t,s1,s2);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessage3(t : longint;const s1,s2,s3 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.Message3(t,s1,s2,s3);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;


    procedure cgmessagepos(const pos:tfileposinfo;t : longint);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.MessagePos(pos,t);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessagepos1(const pos:tfileposinfo;t : longint;const s1 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.MessagePos1(pos,t,s1);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessagepos2(const pos:tfileposinfo;t : longint;const s1,s2 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.MessagePos2(pos,t,s1,s2);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;

    procedure cgmessagepos3(const pos:tfileposinfo;t : longint;const s1,s2,s3 : string);
      var
         olderrorcount : longint;
      begin
         if not(codegenerror) then
           begin
              olderrorcount:=Errorcount;
              verbose.MessagePos3(pos,t,s1,s2,s3);
              codegenerror:=olderrorcount<>Errorcount;
           end;
      end;


    procedure FlushOutput;
      begin
        if not (Status.Use_StdErr) then (* StdErr is flushed after every line *)
          begin
            if Status.Use_Redir then
              Flush(Status.RedirFile)
            else
              Flush(Output);
          end;
      end;


{*****************************************************************************
                                Initialization
*****************************************************************************}

    procedure InitVerbose;
      begin
      { Init }
        msg:=new(pmessage,Init(20,msgidxmax));
        if msg=nil then
         begin
           writeln('Fatal: MsgIdx Wrong');
           halt(3);
         end;
{$ifndef EXTERN_MSG}
        msg^.LoadIntern(@msgtxt,msgtxtsize);
{$else EXTERN_MSG}
        LoadMsgFile(exepath+'errore.msg');
{$endif EXTERN_MSG}
        FillChar(Status,sizeof(TCompilerStatus),0);
        status.verbosity:=V_Default;
        Status.MaxErrorCount:=50;
        Status.codesize:=aword(-1);
        Status.datasize:=aword(-1);
        Loadprefixes;
        lastfileidx:=-1;
        lastmoduleidx:=-1;
        status.currentmodule:='';
        status.currentsource:='';
        status.currentsourcepath:='';
        { Register internalerrorproc for cutils/cclasses }
        internalerrorproc:=@internalerror;
      end;


    procedure DoneVerbose;
      begin
        if assigned(msg) then
         begin
           dispose(msg,Done);
           msg:=nil;
         end;
        DoneRedirectFile;
      end;


initialization
  constexp.internalerror:=@internalerror;
finalization
  { Be sure to close the redirect files to flush all data }
  DoneRedirectFile;
end.
