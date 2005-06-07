{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                    subroutines For TextFile handling
****************************************************************************}

Procedure FileCloseFunc(Var t:TextRec);
Begin
  Do_Close(t.Handle);
  t.Handle:=UnusedHandle;
End;

Procedure FileReadFunc(var t:TextRec);
Begin
  t.BufEnd:=Do_Read(t.Handle,t.Bufptr,t.BufSize);
  t.BufPos:=0;
End;


Procedure FileWriteFunc(var t:TextRec);
var
  i : longint;
Begin
  i:=Do_Write(t.Handle,t.Bufptr,t.BufPos);
  if i<>t.BufPos then
    InOutRes:=101;
  t.BufPos:=0;
End;


Procedure FileOpenFunc(var t:TextRec);
var
  Flags : Longint;
Begin
  Case t.mode Of
    fmInput : Flags:=$10000;
   fmOutput : Flags:=$11001;
   fmAppend : Flags:=$10101;
  else
   begin
     InOutRes:=102;
     exit;
   end;
  End;
  Do_Open(t,PChar(@t.Name),Flags);
  t.CloseFunc:=@FileCloseFunc;
  t.FlushFunc:=nil;
  if t.Mode=fmInput then
   t.InOutFunc:=@FileReadFunc
  else
   begin
     t.InOutFunc:=@FileWriteFunc;
   { Only install flushing if its a NOT a file, and only check if there
     was no error opening the file, becuase else we always get a bad
     file handle error 6 (PFV) }
     if (InOutRes=0) and
        Do_Isdevice(t.Handle) then
      t.FlushFunc:=@FileWriteFunc;
   end;
End;


Procedure assign(var t:Text;const s:String);
Begin
  FillChar(t,SizeOf(TextRec),0);
{ only set things that are not zero }
  TextRec(t).Handle:=UnusedHandle;
  TextRec(t).mode:=fmClosed;
  TextRec(t).BufSize:=TextRecBufSize;
  TextRec(t).Bufptr:=@TextRec(t).Buffer;
  TextRec(t).OpenFunc:=@FileOpenFunc;
  TextRec(t).LineEnd:=LineEnding;
  Move(s[1],TextRec(t).Name,Length(s));
End;


Procedure assign(var t:Text;p:pchar);
begin
  Assign(t,StrPas(p));
end;


Procedure assign(var t:Text;c:char);
begin
  Assign(t,string(c));
end;


Procedure Close(var t : Text);[IOCheck];
Begin
  if InOutRes<>0 then
   Exit;
  case TextRec(t).mode of
    fmInput,fmOutPut,fmAppend:
      Begin
        { Write pending buffer }
        If Textrec(t).Mode=fmoutput then
          FileFunc(TextRec(t).InOutFunc)(TextRec(t));
        { Only close functions not connected to stdout.}
        If ((TextRec(t).Handle<>StdInputHandle) and
            (TextRec(t).Handle<>StdOutputHandle) and
            (TextRec(t).Handle<>StdErrorHandle)) Then
          FileFunc(TextRec(t).CloseFunc)(TextRec(t));
        TextRec(t).mode := fmClosed;
        { Reset buffer for safety }
        TextRec(t).BufPos:=0;
        TextRec(t).BufEnd:=0;
      End
    else inOutRes := 103;
  End;
End;


Procedure OpenText(var t : Text;mode,defHdl:Longint);
Begin
  Case TextRec(t).mode Of {This gives the fastest code}
   fmInput,fmOutput,fmInOut : Close(t);
   fmClosed : ;
  else
   Begin
     InOutRes:=102;
     exit;
   End;
  End;
  TextRec(t).mode:=mode;
  TextRec(t).bufpos:=0;
  TextRec(t).bufend:=0;
  FileFunc(TextRec(t).OpenFunc)(TextRec(t));
  { reset the mode to closed when an error has occured }
  if InOutRes<>0 then
   TextRec(t).mode:=fmClosed;
End;


Procedure Rewrite(var t : Text);[IOCheck];
Begin
  If InOutRes<>0 then
   exit;
  OpenText(t,fmOutput,1);
End;


Procedure Reset(var t : Text);[IOCheck];
Begin
  If InOutRes<>0 then
   exit;
  OpenText(t,fmInput,0);
End;


Procedure Append(var t : Text);[IOCheck];
Begin
  If InOutRes<>0 then
   exit;
  OpenText(t,fmAppend,1);
End;


Procedure Flush(var t : Text);[IOCheck];
Begin
  If InOutRes<>0 then
   exit;
  if TextRec(t).mode<>fmOutput then
   begin
     if TextRec(t).mode=fmInput then
      InOutRes:=105
     else
      InOutRes:=103;
     exit;
   end;
{ Not the flushfunc but the inoutfunc should be used, becuase that
  writes the data, flushfunc doesn't need to be assigned }
  FileFunc(TextRec(t).InOutFunc)(TextRec(t));
End;


Procedure Erase(var t:Text);[IOCheck];
Begin
  If InOutRes <> 0 then
   exit;
  If TextRec(t).mode=fmClosed Then
   Do_Erase(PChar(@TextRec(t).Name));
End;


Procedure Rename(var t : text;p:pchar);[IOCheck];
Begin
  If InOutRes <> 0 then
   exit;
  If TextRec(t).mode=fmClosed Then
   Begin
     Do_Rename(PChar(@TextRec(t).Name),p);
     { check error code of do_rename }
     If InOutRes = 0 then
         Move(p^,TextRec(t).Name,StrLen(p)+1);
   End;
End;


Procedure Rename(var t : Text;const s : string);[IOCheck];
var
  p : array[0..255] Of Char;
Begin
  If InOutRes <> 0 then
   exit;
  Move(s[1],p,Length(s));
  p[Length(s)]:=#0;
  Rename(t,Pchar(@p));
End;


Procedure Rename(var t : Text;c : char);[IOCheck];
var
  p : array[0..1] Of Char;
Begin
  If InOutRes <> 0 then
   exit;
  p[0]:=c;
  p[1]:=#0;
  Rename(t,Pchar(@p));
End;


Function Eof(Var t: Text): Boolean;[IOCheck];
Begin
  If (InOutRes<>0) then
   exit(true);
  if (TextRec(t).mode<>fmInput) Then
   begin
     if TextRec(t).mode=fmOutput then
      InOutRes:=104
     else
      InOutRes:=103;
     exit(true);
   end;
  If TextRec(t).BufPos>=TextRec(t).BufEnd Then
   begin
     FileFunc(TextRec(t).InOutFunc)(TextRec(t));
     If TextRec(t).BufPos>=TextRec(t).BufEnd Then
      exit(true);
   end;
  Eof:=CtrlZMarksEOF and (TextRec(t).Bufptr^[TextRec(t).BufPos]=#26);
end;


Function Eof:Boolean;
Begin
  Eof:=Eof(Input);
End;


Function SeekEof (Var t : Text) : Boolean;
var
  oldfilepos, oldbufpos, oldbufend, reads: longint;
  isdevice: boolean;
Begin
  If (InOutRes<>0) then
   exit(true);
  if (TextRec(t).mode<>fmInput) Then
   begin
     if TextRec(t).mode=fmOutPut then
      InOutRes:=104
     else
      InOutRes:=103;
     exit(true);
   end;
  { try to save the current position in the file, seekeof() should not move }
  { the current file position (JM)                                          }
  oldbufpos := TextRec(t).BufPos;
  oldbufend := TextRec(t).BufEnd;
  reads := 0;
  oldfilepos := -1;
  isdevice := Do_IsDevice(TextRec(t).handle);
  repeat
    If TextRec(t).BufPos>=TextRec(t).BufEnd Then
     begin
       { signal that the we will have to do a seek }
       inc(reads);
       if not isdevice and
          (reads = 1) then
         begin
           oldfilepos := Do_FilePos(TextRec(t).handle) - TextRec(t).BufEnd;
           InOutRes:=0;
         end;
       FileFunc(TextRec(t).InOutFunc)(TextRec(t));
       If TextRec(t).BufPos>=TextRec(t).BufEnd Then
        begin
          { if we only did a read in which we didn't read anything, the }
          { old buffer is still valid and we can simply restore the     }
          { pointers (JM)                                               }
          dec(reads);
          SeekEof := true;
          break;
        end;
     end;
    case TextRec(t).Bufptr^[TextRec(t).BufPos] of
         #26 : if CtrlZMarksEOF then
                begin
                 SeekEof := true;
                 break;
                end;
     #10,#13,
      #9,' ' : ;
    else
     begin
       SeekEof := false;
       break;
     end;
    end;
   inc(TextRec(t).BufPos);
  until false;
  { restore file position if not working with a device }
  if not isdevice then
    { if we didn't modify the buffer, simply restore the BufPos and BufEnd  }
    { (the latter becuase it's now probably set to zero because nothing was }
    {  was read anymore)                                                    }
    if (reads = 0) then
      begin
        TextRec(t).BufPos:=oldbufpos;
        TextRec(t).BufEnd:=oldbufend;
      end
    { otherwise return to the old filepos and reset the buffer }
    else
      begin
        do_seek(TextRec(t).handle,oldfilepos);
        InOutRes:=0;
        FileFunc(TextRec(t).InOutFunc)(TextRec(t));
        TextRec(t).BufPos:=oldbufpos;
      end;
End;


Function SeekEof : Boolean;
Begin
  SeekEof:=SeekEof(Input);
End;


Function Eoln(var t:Text) : Boolean;
Begin
  If (InOutRes<>0) then
   exit(true);
  if (TextRec(t).mode<>fmInput) Then
   begin
     if TextRec(t).mode=fmOutPut then
      InOutRes:=104
     else
      InOutRes:=103;
     exit(true);
   end;
  If TextRec(t).BufPos>=TextRec(t).BufEnd Then
   begin
     FileFunc(TextRec(t).InOutFunc)(TextRec(t));
     If TextRec(t).BufPos>=TextRec(t).BufEnd Then
      exit(true);
   end;
  if CtrlZMarksEOF and (TextRec (T).BufPtr^[TextRec (T).BufPos] = #26) then
   exit (true);
  Eoln:=(TextRec(t).Bufptr^[TextRec(t).BufPos] in [#10,#13]);
End;


Function Eoln : Boolean;
Begin
  Eoln:=Eoln(Input);
End;


Function SeekEoln (Var t : Text) : Boolean;
Begin
  If (InOutRes<>0) then
   exit(true);
  if (TextRec(t).mode<>fmInput) Then
   begin
     if TextRec(t).mode=fmOutput then
      InOutRes:=104
     else
      InOutRes:=103;
     exit(true);
   end;
  repeat
    If TextRec(t).BufPos>=TextRec(t).BufEnd Then
     begin
       FileFunc(TextRec(t).InOutFunc)(TextRec(t));
       If TextRec(t).BufPos>=TextRec(t).BufEnd Then
        exit(true);
     end;
    case TextRec(t).Bufptr^[TextRec(t).BufPos] of
         #26: if CtrlZMarksEOF then
               exit (true);
     #10,#13 : exit(true);
      #9,' ' : ;
    else
     exit(false);
    end;
    inc(TextRec(t).BufPos);
  until false;
End;


Function SeekEoln : Boolean;
Begin
  SeekEoln:=SeekEoln(Input);
End;


{$ifndef INTERNCONSTINTF}
Procedure SetTextBuf(Var F : Text; Var Buf);[INTERNPROC: fpc_In_settextbuf_file_x];
{$endif}


Procedure SetTextBuf(Var F : Text; Var Buf; Size : Longint);
Begin
  TextRec(f).BufPtr:=@Buf;
  TextRec(f).BufSize:=Size;
  TextRec(f).BufPos:=0;
  TextRec(f).BufEnd:=0;
End;

Procedure SetTextLineEnding(Var f:Text; Ending:string);
Begin
  TextRec(F).LineEnd:=Ending;
End;


Function fpc_get_input:PText;{$ifdef hascompilerproc}compilerproc;{$endif}
begin
  fpc_get_input:=@Input;
end;


Function fpc_get_output:PText;{$ifdef hascompilerproc}compilerproc;{$endif}
begin
  fpc_get_output:=@Output;
end;


{*****************************************************************************
                               Write(Ln)
*****************************************************************************}

Procedure fpc_WriteBuffer(var f:Text;const b;len:longint);[Public,Alias:'FPC_WRITEBUFFER'];
var
  p   : pchar;
  left,
  idx : longint;
begin
  p:=pchar(@b);
  idx:=0;
  left:=TextRec(f).BufSize-TextRec(f).BufPos;
  while len>left do
   begin
     move(p[idx],TextRec(f).Bufptr^[TextRec(f).BufPos],left);
     dec(len,left);
     inc(idx,left);
     inc(TextRec(f).BufPos,left);
     FileFunc(TextRec(f).InOutFunc)(TextRec(f));
     left:=TextRec(f).BufSize-TextRec(f).BufPos;
   end;
  move(p[idx],TextRec(f).Bufptr^[TextRec(f).BufPos],len);
  inc(TextRec(f).BufPos,len);
end;


Procedure fpc_WriteBlanks(var f:Text;len:longint);[Public,Alias:'FPC_WRITEBLANKS'];
var
  left : longint;
begin
  left:=TextRec(f).BufSize-TextRec(f).BufPos;
  while len>left do
   begin
     FillChar(TextRec(f).Bufptr^[TextRec(f).BufPos],left,' ');
     dec(len,left);
     inc(TextRec(f).BufPos,left);
     FileFunc(TextRec(f).InOutFunc)(TextRec(f));
     left:=TextRec(f).BufSize-TextRec(f).BufPos;
   end;
  FillChar(TextRec(f).Bufptr^[TextRec(f).BufPos],len,' ');
  inc(TextRec(f).BufPos,len);
end;


Procedure fpc_Write_End(var f:Text);[Public,Alias:'FPC_WRITE_END']; iocheck; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  if TextRec(f).FlushFunc<>nil then
   FileFunc(TextRec(f).FlushFunc)(TextRec(f));
end;


Procedure fpc_Writeln_End(var f:Text);[Public,Alias:'FPC_WRITELN_END']; iocheck; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  If InOutRes <> 0 then exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        { Write EOL }
        fpc_WriteBuffer(f,TextRec(f).LineEnd[1],length(TextRec(f).LineEnd));
        { Flush }
        if TextRec(f).FlushFunc<>nil then
          FileFunc(TextRec(f).FlushFunc)(TextRec(f));
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
end;


Procedure fpc_Write_Text_ShortStr(Len : Longint;var f : Text;const s : String); iocheck; [Public,Alias:'FPC_WRITE_TEXT_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Begin
  If (InOutRes<>0) then
   exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        If Len>Length(s) Then
          fpc_WriteBlanks(f,Len-Length(s));
        fpc_WriteBuffer(f,s[1],Length(s));
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
End;

{ provide local access to write_str }
procedure Write_Str(Len : Longint;var f : Text;const s : String); iocheck; [external name 'FPC_WRITE_TEXT_SHORTSTR'];


Procedure fpc_Write_Text_Pchar_as_Array(Len : Longint;var f : Text;const s : array of char); iocheck; [Public,Alias:'FPC_WRITE_TEXT_PCHAR_AS_ARRAY']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  ArrayLen : longint;
  p : pchar;
Begin
  If (InOutRes<>0) then
   exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        p:=pchar(@s);
        { can't use StrLen, since that one could try to read past the end }
        { of the heap (JM)                                                }
        ArrayLen:=IndexByte(p^,high(s)+1,0);
        { IndexByte returns -1 if not found (JM) }
        if ArrayLen = -1 then
          ArrayLen := high(s)+1;
        If Len>ArrayLen Then
          fpc_WriteBlanks(f,Len-ArrayLen);
        fpc_WriteBuffer(f,p^,ArrayLen);
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
End;


Procedure fpc_Write_Text_PChar_As_Pointer(Len : Longint;var f : Text;p : PChar); iocheck; [Public,Alias:'FPC_WRITE_TEXT_PCHAR_AS_POINTER']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  PCharLen : longint;
Begin
  If (p=nil) or (InOutRes<>0) then
   exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        PCharLen:=StrLen(p);
        If Len>PCharLen Then
          fpc_WriteBlanks(f,Len-PCharLen);
        fpc_WriteBuffer(f,p^,PCharLen);
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
End;


Procedure fpc_Write_Text_AnsiStr (Len : Longint; Var f : Text; S : AnsiString); iocheck; [Public,alias:'FPC_WRITE_TEXT_ANSISTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
{
 Writes a AnsiString to the Text file T
}
var
  SLen : longint;
begin
  If (InOutRes<>0) then
   exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        SLen:=Length(s);
        If Len>SLen Then
          fpc_WriteBlanks(f,Len-SLen);
        if slen > 0 then
          fpc_WriteBuffer(f,PChar(S)^,SLen);
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
end;


Procedure fpc_Write_Text_WideStr (Len : Longint; Var f : Text; S : WideString); iocheck; [Public,alias:'FPC_WRITE_TEXT_WIDESTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
{
 Writes a WideString to the Text file T
}
var
  SLen : longint;
begin
  If (pointer(S)=nil) or (InOutRes<>0) then
   exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        SLen:=Length(s);
        If Len>SLen Then
          fpc_WriteBlanks(f,Len-SLen);
        fpc_WriteBuffer(f,PChar(AnsiString(S))^,SLen);
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
end;

Procedure fpc_Write_Text_SInt(Len : Longint;var t : Text;l : ValSInt); iocheck; [Public,Alias:'FPC_WRITE_TEXT_SINT']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  s : String;
Begin
  If (InOutRes<>0) then
   exit;
  Str(l,s);
  Write_Str(Len,t,s);
End;


Procedure fpc_Write_Text_UInt(Len : Longint;var t : Text;l : ValUInt); iocheck; [Public,Alias:'FPC_WRITE_TEXT_UINT']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  s : String;
Begin
  If (InOutRes<>0) then
   exit;
  Str(L,s);
  Write_Str(Len,t,s);
End;


{$ifndef CPU64}

procedure fpc_write_text_qword(len : longint;var t : text;q : qword); iocheck; [public,alias:'FPC_WRITE_TEXT_QWORD']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  s : string;
begin
  if (InOutRes<>0) then
   exit;
  str(q,s);
  write_str(len,t,s);
end;

procedure fpc_write_text_int64(len : longint;var t : text;i : int64); iocheck; [public,alias:'FPC_WRITE_TEXT_INT64']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  s : string;
begin
  if (InOutRes<>0) then
   exit;
  str(i,s);
  write_str(len,t,s);
end;

{$endif CPU64}

Procedure fpc_Write_Text_Float(rt,fixkomma,Len : Longint;var t : Text;r : ValReal); iocheck; [Public,Alias:'FPC_WRITE_TEXT_FLOAT']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  s : String;
Begin
  If (InOutRes<>0) then
   exit;
  Str_real(Len,fixkomma,r,treal_type(rt),s);
  Write_Str(Len,t,s);
End;


Procedure fpc_Write_Text_Boolean(Len : Longint;var t : Text;b : Boolean); iocheck; [Public,Alias:'FPC_WRITE_TEXT_BOOLEAN']; {$ifdef hascompilerproc} compilerproc; {$endif}
Begin
  If (InOutRes<>0) then
   exit;
{ Can't use array[boolean] because b can be >0 ! }
  if b then
    Write_Str(Len,t,'TRUE')
  else
    Write_Str(Len,t,'FALSE');
End;


Procedure fpc_Write_Text_Char(Len : Longint;var t : Text;c : Char); iocheck; [Public,Alias:'FPC_WRITE_TEXT_CHAR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Begin
  If (InOutRes<>0) then
    exit;
  if (TextRec(t).mode<>fmOutput) Then
   begin
     if TextRec(t).mode=fmClosed then
      InOutRes:=103
     else
      InOutRes:=105;
     exit;
   end;
  If Len>1 Then
    fpc_WriteBlanks(t,Len-1);
  If TextRec(t).BufPos+1>=TextRec(t).BufSize Then
    FileFunc(TextRec(t).InOutFunc)(TextRec(t));
  TextRec(t).Bufptr^[TextRec(t).BufPos]:=c;
  Inc(TextRec(t).BufPos);
End;


Procedure fpc_Write_Text_WideChar(Len : Longint;var t : Text;c : WideChar); iocheck; [Public,Alias:'FPC_WRITE_TEXT_WIDECHAR']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  ch : char;
Begin
  If (InOutRes<>0) then
    exit;
  if (TextRec(t).mode<>fmOutput) Then
   begin
     if TextRec(t).mode=fmClosed then
      InOutRes:=103
     else
      InOutRes:=105;
     exit;
   end;
  If Len>1 Then
    fpc_WriteBlanks(t,Len-1);
  If TextRec(t).BufPos+1>=TextRec(t).BufSize Then
    FileFunc(TextRec(t).InOutFunc)(TextRec(t));
  ch:=c;
  TextRec(t).Bufptr^[TextRec(t).BufPos]:=ch;
  Inc(TextRec(t).BufPos);
End;


{*****************************************************************************
                                Read(Ln)
*****************************************************************************}

Function NextChar(var f:Text;var s:string):Boolean;
begin
  NextChar:=false;
  if (TextRec(f).BufPos<TextRec(f).BufEnd) then
   if not (CtrlZMarksEOF) or (TextRec(f).Bufptr^[TextRec(f).BufPos]<>#26) then
    begin
     if length(s)<high(s) then
      begin
        inc(s[0]);
        s[length(s)]:=TextRec(f).BufPtr^[TextRec(f).BufPos];
      end;
     Inc(TextRec(f).BufPos);
     If TextRec(f).BufPos>=TextRec(f).BufEnd Then
      FileFunc(TextRec(f).InOutFunc)(TextRec(f));
     NextChar:=true;
   end;
end;


Function IgnoreSpaces(var f:Text):Boolean;
{
  Removes all leading spaces,tab,eols from the input buffer, returns true if
  the buffer is empty
}
var
  s : string;
begin
  s:='';
  IgnoreSpaces:=false;
  { Return false when already at EOF }
  if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
   exit;
(* Check performed separately to avoid accessing memory outside buffer *)
  if CtrlZMarksEOF and (TextRec(f).Bufptr^[TextRec(f).BufPos]=#26) then
   exit;
  while (TextRec(f).Bufptr^[TextRec(f).BufPos] <= ' ') do
   begin
     if not NextChar(f,s) then
      exit;
     { EOF? }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      break;
     if CtrlZMarksEOF and (TextRec(f).Bufptr^[TextRec(f).BufPos]=#26) then
      break;
   end;
  IgnoreSpaces:=true;
end;


procedure ReadNumeric(var f:Text;var s:string);
{
  Read numeric input, if buffer is empty then return True
}
begin
  repeat
    if not NextChar(f,s) then
      exit;
  until (length(s)=high(s)) or (TextRec(f).BufPtr^[TextRec(f).BufPos] <= ' ');
end;


Procedure fpc_Read_End(var f:Text);[Public,Alias:'FPC_READ_END']; iocheck; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
  if TextRec(f).FlushFunc<>nil then
   FileFunc(TextRec(f).FlushFunc)(TextRec(f));
end;


Procedure fpc_ReadLn_End(var f : Text);[Public,Alias:'FPC_READLN_END']; iocheck; {$ifdef hascompilerproc} compilerproc; {$endif}
var prev: char;
Begin
{ Check error and if file is open and load buf if empty }
  If (InOutRes<>0) then
   exit;
  if (TextRec(f).mode<>fmInput) Then
   begin
     case TextRec(f).mode of
      fmOutPut,fmAppend:
        InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  if TextRec(f).BufPos>=TextRec(f).BufEnd Then
   begin
     FileFunc(TextRec(f).InOutFunc)(TextRec(f));
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
       { Flush if set }
       begin
         if (TextRec(f).FlushFunc<>nil) then
           FileFunc(TextRec(f).FlushFunc)(TextRec(f));
         exit;
       end;
   end;
  if CtrlZMarksEOF and (TextRec (F).BufPtr^ [TextRec (F).BufPos] = #26) then
   Exit;
  repeat
    prev := TextRec(f).BufPtr^[TextRec(f).BufPos];
    inc(TextRec(f).BufPos);
{ no system uses #10#13 as line seperator (#10 = *nix, #13 = Mac, }
{ #13#10 = Dos), so if we've got #10, we can safely exit          }
    if prev = #10 then
      exit;
    {$ifdef MACOS}
    if prev = #13 then
      {StdInput on macos never have dos line ending, so this is safe.}
      if TextRec(f).Handle = StdInputHandle then
        exit;
    {$endif MACOS}
    if TextRec(f).BufPos>=TextRec(f).BufEnd Then
      begin
        FileFunc(TextRec(f).InOutFunc)(TextRec(f));
        if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
          { Flush if set }
          begin
           if (TextRec(f).FlushFunc<>nil) then
             FileFunc(TextRec(f).FlushFunc)(TextRec(f));
           exit;
         end;
      end;
   if CtrlZMarksEOF and (TextRec (F).BufPtr^ [TextRec (F).BufPos] = #26) then
    Exit;
   if (prev=#13) then
     { is there also a #10 after it? }
     begin
       if (TextRec(f).BufPtr^[TextRec(f).BufPos]=#10) then
         { yes, skip that one as well }
         inc(TextRec(f).BufPos);
       exit;
     end;
  until false;
End;


Function ReadPCharLen(var f:Text;s:pchar;maxlen:longint):longint;
var
  sPos,len : Longint;
  p,startp,maxp : pchar;
Begin
  ReadPCharLen:=0;
{ Check error and if file is open }
  If (InOutRes<>0) then
   exit;
  if (TextRec(f).mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
{ Read maximal until Maxlen is reached }
  sPos:=0;
  repeat
    If TextRec(f).BufPos>=TextRec(f).BufEnd Then
     begin
       FileFunc(TextRec(f).InOutFunc)(TextRec(f));
       If TextRec(f).BufPos>=TextRec(f).BufEnd Then
         break;
     end;
    p:=@TextRec(f).Bufptr^[TextRec(f).BufPos];
    if SPos+TextRec(f).BufEnd-TextRec(f).BufPos>MaxLen then
     maxp:=@TextRec(f).BufPtr^[TextRec(f).BufPos+MaxLen-SPos]
    else
     maxp:=@TextRec(f).Bufptr^[TextRec(f).BufEnd];
    startp:=p;
  { search linefeed }
    while (p<maxp) and not(P^ in [#10,#13]) do
     inc(p);
  { calculate read bytes }
    len:=p-startp;
    inc(TextRec(f).BufPos,Len);
    Move(startp^,s[sPos],Len);
    inc(sPos,Len);
  { was it a LF or CR? then leave }
    if (spos=MaxLen) or
       ((p<maxp) and (p^ in [#10,#13])) then
      break;
  until false;
  ReadPCharLen:=spos;
End;


Procedure fpc_Read_Text_ShortStr(var f : Text;out s : String); iocheck; [Public,Alias:'FPC_READ_TEXT_SHORTSTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
Begin
  s[0]:=chr(ReadPCharLen(f,pchar(@s[1]),high(s)));
End;


Procedure fpc_Read_Text_PChar_As_Pointer(var f : Text;out s : PChar); iocheck; [Public,Alias:'FPC_READ_TEXT_PCHAR_AS_POINTER']; {$ifdef hascompilerproc} compilerproc; {$endif}
Begin
  pchar(s+ReadPCharLen(f,s,$7fffffff))^:=#0;
End;


Procedure fpc_Read_Text_PChar_As_Array(var f : Text;out s : array of char); iocheck; [Public,Alias:'FPC_READ_TEXT_PCHAR_AS_ARRAY']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  len: longint;
Begin
  len := ReadPCharLen(f,pchar(@s),high(s)+1);
  if len <= high(s) then
    s[len] := #0;
End;


Procedure fpc_Read_Text_AnsiStr(var f : Text;out s : AnsiString); iocheck; [Public,Alias:'FPC_READ_TEXT_ANSISTR']; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  slen,len : longint;
Begin
  slen:=0;
  Repeat
    // SetLength will reallocate the length.
    SetLength(S,slen+255);
    len:=ReadPCharLen(f,pchar(Pointer(S)+slen),255);
    inc(slen,len);
  Until len<255;
  // Set actual length
  SetLength(S,Slen);
End;

{$ifdef hascompilerproc}
procedure fpc_Read_Text_Char(var f : Text; out c: char); iocheck; [Public,Alias:'FPC_READ_TEXT_CHAR'];compilerproc;
{$else hascompilerproc}
Function fpc_Read_Text_Char(var f : Text):char;[Public,Alias:'FPC_READ_TEXT_CHAR'];
{$endif hascompilerproc}
Begin
{$ifdef hascompilerproc}
  c:=#0;
{$else hascompilerproc}
  fpc_Read_Text_Char:=#0;
{$endif hascompilerproc}
{ Check error and if file is open }
  If (InOutRes<>0) then
   exit;
  if (TextRec(f).mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
{ Read next char or EOF }
  If TextRec(f).BufPos>=TextRec(f).BufEnd Then
   begin
     FileFunc(TextRec(f).InOutFunc)(TextRec(f));
     If TextRec(f).BufPos>=TextRec(f).BufEnd Then
{$ifdef hascompilerproc}
       begin
         c := #26;
         exit;
       end;
{$else hascompilerproc}
       exit(#26);
{$endif hascompilerproc}
   end;
{$ifdef hascompilerproc}
  c:=TextRec(f).Bufptr^[TextRec(f).BufPos];
{$else hascompilerproc}
  fpc_Read_Text_Char:=TextRec(f).Bufptr^[TextRec(f).BufPos];
{$endif hascompilerproc}
  inc(TextRec(f).BufPos);
end;


{$ifdef hascompilerproc}
Procedure fpc_Read_Text_SInt(var f : Text; out l : ValSInt); iocheck; [Public,Alias:'FPC_READ_TEXT_SINT']; compilerproc;
{$else hascompilerproc}
Function fpc_Read_Text_SInt(var f : Text):ValSInt;[Public,Alias:'FPC_READ_TEXT_SINT'];
{$endif hascompilerproc}
var
  hs   : String;
  code : longint;
Begin
{$ifdef hascompilerproc}
  l:=0;
{$else hascompilerproc}
  fpc_Read_Text_SInt:=0;
{$endif hascompilerproc}
{ Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (TextRec(f).mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  If TextRec(f).BufPos>=TextRec(f).BufEnd Then
   FileFunc(TextRec(f).InOutFunc)(TextRec(f));
  hs:='';
  if IgnoreSpaces(f) then
   begin
     { When spaces were found and we are now at EOF,
       then we return 0 }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      exit;
     if CtrlZMarksEOF and (TextRec(f).Bufptr^[TextRec(f).BufPos]=#26) then
      exit;
     ReadNumeric(f,hs);
   end;
{$ifdef hascompilerproc}
   if (hs = '') then
    L := 0
   else
    begin
     Val(hs,l,code);
     if Code <> 0 then
      InOutRes:=106;
    end;
{$else hascompilerproc}
   if (hs = '') then
    fpc_Read_Text_SInt := 0
   else
    begin
     Val(hs,fpc_Read_Text_SInt,code);
     if Code <> 0 then
      InOutRes:=106;
    end;
{$endif hascompilerproc}
End;


{$ifdef hascompilerproc}
Procedure fpc_Read_Text_UInt(var f : Text; out u : ValUInt);  iocheck; [Public,Alias:'FPC_READ_TEXT_UINT']; compilerproc;
{$else hascompilerproc}
Function fpc_Read_Text_UInt(var f : Text):ValUInt;[Public,Alias:'FPC_READ_TEXT_UINT'];
{$endif hascompilerproc}
var
  hs   : String;
  code : longint;
Begin
{$ifdef hascompilerproc}
  u:=0;
{$else hascompilerproc}
  fpc_Read_Text_UInt:=0;
{$endif hascompilerproc}
{ Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (TextRec(f).mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  If TextRec(f).BufPos>=TextRec(f).BufEnd Then
   FileFunc(TextRec(f).InOutFunc)(TextRec(f));
  hs:='';
  if IgnoreSpaces(f) then
   begin
     { When spaces were found and we are now at EOF,
       then we return 0 }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      exit;
     ReadNumeric(f,hs);
   end;
{$ifdef hascompilerproc}
  val(hs,u,code);
{$else hascompilerproc}
  val(hs,fpc_Read_Text_UInt,code);
{$endif hascompilerproc}
  If code<>0 Then
   InOutRes:=106;
End;


{$ifdef hascompilerproc}
procedure fpc_Read_Text_Float(var f : Text; out v : ValReal); iocheck; [Public,Alias:'FPC_READ_TEXT_FLOAT']; compilerproc;
{$else hascompilerproc}
Function fpc_Read_Text_Float(var f : Text):ValReal;[Public,Alias:'FPC_READ_TEXT_FLOAT'];
{$endif hascompilerproc}
var
  hs : string;
  code : Word;
begin
{$ifdef hascompilerproc}
  v:=0.0;
{$else hascompilerproc}
  fpc_Read_Text_Float:=0.0;
{$endif hascompilerproc}
{ Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (TextRec(f).mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  If TextRec(f).BufPos>=TextRec(f).BufEnd Then
   FileFunc(TextRec(f).InOutFunc)(TextRec(f));
  hs:='';
  if IgnoreSpaces(f) then
   begin
     { When spaces were found and we are now at EOF,
       then we return 0 }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      exit;
     ReadNumeric(f,hs);
   end;
{$ifdef hascompilerproc}
  val(hs,v,code);
{$else hascompilerproc}
  val(hs,fpc_Read_Text_Float,code);
{$endif hascompilerproc}
  If code<>0 Then
   InOutRes:=106;
end;


{$ifndef cpu64}

{$ifdef hascompilerproc}
procedure fpc_Read_Text_QWord(var f : text; out q : qword); iocheck; [public,alias:'FPC_READ_TEXT_QWORD']; compilerproc;
{$else hascompilerproc}
function fpc_Read_Text_QWord(var f : text) : qword;[public,alias:'FPC_READ_TEXT_QWORD'];
{$endif hascompilerproc}
var
  hs   : String;
  code : longint;
Begin
{$ifdef hascompilerproc}
  q:=0;
{$else hascompilerproc}
  fpc_Read_Text_QWord:=0;
{$endif hascompilerproc}
  { Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (TextRec(f).mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  If TextRec(f).BufPos>=TextRec(f).BufEnd Then
   FileFunc(TextRec(f).InOutFunc)(TextRec(f));
  hs:='';
  if IgnoreSpaces(f) then
   begin
     { When spaces were found and we are now at EOF,
       then we return 0 }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      exit;
     ReadNumeric(f,hs);
   end;
{$ifdef hascompilerproc}
  val(hs,q,code);
{$else hascompilerproc}
  val(hs,fpc_Read_Text_QWord,code);
{$endif hascompilerproc}
  If code<>0 Then
   InOutRes:=106;
End;

{$ifdef hascompilerproc}
procedure fpc_Read_Text_Int64(var f : text; out i : int64); iocheck; [public,alias:'FPC_READ_TEXT_INT64']; compilerproc;
{$else hascompilerproc}
function fpc_Read_Text_Int64(var f : text) : int64;[public,alias:'FPC_READ_TEXT_INT64']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$endif hascompilerproc}
var
  hs   : String;
  code : Longint;
Begin
{$ifdef hascompilerproc}
  i:=0;
{$else hascompilerproc}
  fpc_Read_Text_Int64:=0;
{$endif hascompilerproc}
{ Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (TextRec(f).mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  If TextRec(f).BufPos>=TextRec(f).BufEnd Then
   FileFunc(TextRec(f).InOutFunc)(TextRec(f));
  hs:='';
  if IgnoreSpaces(f) then
   begin
     { When spaces were found and we are now at EOF,
       then we return 0 }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      exit;
     ReadNumeric(f,hs);
   end;
{$ifdef hascompilerproc}
  Val(hs,i,code);
{$else hascompilerproc}
  Val(hs,fpc_Read_Text_Int64,code);
{$endif hascompilerproc}
  If code<>0 Then
   InOutRes:=106;
End;

{$endif CPU64}


{*****************************************************************************
                               Initializing
*****************************************************************************}

procedure OpenStdIO(var f:text;mode,hdl:longint);
begin
  Assign(f,'');
  TextRec(f).Handle:=hdl;
  TextRec(f).Mode:=mode;
  TextRec(f).Closefunc:=@FileCloseFunc;
  case mode of
    fmInput :
      TextRec(f).InOutFunc:=@FileReadFunc;
    fmOutput :
      begin
        TextRec(f).InOutFunc:=@FileWriteFunc;
        TextRec(f).FlushFunc:=@FileWriteFunc;
      end;
  else
   HandleError(102);
  end;
end;


