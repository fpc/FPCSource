{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Possible Defines:

  EOF_CTRLZ       Is Ctrl-Z (#26) a EOF mark for textfiles
  SHORT_LINEBREAK Use short Linebreaks #10 instead of #10#13

  SHORT_LINEBREAK is defined in the Linux system unit (syslinux.pp)
}

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
  t.BufEnd:=Do_Read(t.Handle,Longint(t.Bufptr),t.BufSize);
  t.BufPos:=0;
End;


Procedure FileWriteFunc(var t:TextRec);
var
  i : longint;
Begin
  i:=Do_Write(t.Handle,Longint(t.Bufptr),t.BufPos);
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
  FillChar(t,SizEof(TextRec),0);
{ only set things that are not zero }
  TextRec(t).Handle:=UnusedHandle;
  TextRec(t).mode:=fmClosed;
  TextRec(t).BufSize:=TextRecBufSize;
  TextRec(t).Bufptr:=@TextRec(t).Buffer;
  TextRec(t).OpenFunc:=@FileOpenFunc;
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
{$ifdef EOF_CTRLZ}
  Eof:=(TextRec(t).Bufptr^[TextRec(t).BufPos]=#26);
{$else}
  Eof:=false;
{$endif EOL_CTRLZ}
end;


Function Eof:Boolean;
Begin
  Eof:=Eof(Input);
End;


Function SeekEof (Var t : Text) : Boolean;
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
  repeat
    If TextRec(t).BufPos>=TextRec(t).BufEnd Then
     begin
       FileFunc(TextRec(t).InOutFunc)(TextRec(t));
       If TextRec(t).BufPos>=TextRec(t).BufEnd Then
        exit(true);
     end;
    case TextRec(t).Bufptr^[TextRec(t).BufPos] of
         #26 : exit(true);
     #10,#13,
      #9,' ' : ;
    else
     exit(false);
    end;
    inc(TextRec(t).BufPos);
  until false;
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
         #26,
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


Procedure SetTextBuf(Var F : Text; Var Buf);[INTERNPROC: In_settextbuf_file_x];


Procedure SetTextBuf(Var F : Text; Var Buf; Size : Longint);
Begin
  TextRec(f).BufPtr:=@Buf;
  TextRec(f).BufSize:=Size;
  TextRec(f).BufPos:=0;
  TextRec(f).BufEnd:=0;
End;


{*****************************************************************************
                               Write(Ln)
*****************************************************************************}

Procedure WriteBuffer(var f:TextRec;var b;len:longint);
var
  p   : pchar;
  left,
  idx : longint;
begin
  p:=pchar(@b);
  idx:=0;
  left:=f.BufSize-f.BufPos;
  while len>left do
   begin
     move(p[idx],f.Bufptr^[f.BufPos],left);
     dec(len,left);
     inc(idx,left);
     inc(f.BufPos,left);
     FileFunc(f.InOutFunc)(f);
     left:=f.BufSize-f.BufPos;
   end;
  move(p[idx],f.Bufptr^[f.BufPos],len);
  inc(f.BufPos,len);
end;


Procedure WriteBlanks(var f:TextRec;len:longint);
var
  left : longint;
begin
  left:=f.BufSize-f.BufPos;
  while len>left do
   begin
     FillChar(f.Bufptr^[f.BufPos],left,' ');
     dec(len,left);
     inc(f.BufPos,left);
     FileFunc(f.InOutFunc)(f);
     left:=f.BufSize-f.BufPos;
   end;
  FillChar(f.Bufptr^[f.BufPos],len,' ');
  inc(f.BufPos,len);
end;


Procedure Write_End(var f:TextRec);[Public,Alias:'FPC_WRITE_END'];
begin
  if f.FlushFunc<>nil then
   FileFunc(f.FlushFunc)(f);
end;


Procedure Writeln_End(var f:TextRec);[Public,Alias:'FPC_WRITELN_END'];
const
{$IFDEF SHORT_LINEBREAK}
  eollen=1;
  eol : array[0..0] of char=(#10);
{$ELSE SHORT_LINEBREAK}
  eollen=2;
  eol : array[0..1] of char=(#13,#10);
{$ENDIF SHORT_LINEBREAK}
begin
  If InOutRes <> 0 then exit;
  case f.mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        { Write EOL }
        WriteBuffer(f,eol,eollen);
        { Flush }
        if f.FlushFunc<>nil then
          FileFunc(f.FlushFunc)(f);
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
end;


Procedure Write_Str(Len : Longint;var f : TextRec;const s : String);[Public,Alias:'FPC_WRITE_TEXT_SHORTSTR'];
Begin
  If (InOutRes<>0) then
   exit;
  case f.mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        If Len>Length(s) Then
          WriteBlanks(f,Len-Length(s));
        WriteBuffer(f,s[1],Length(s));
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
End;


Procedure Write_Array(Len : Longint;var f : TextRec;const s : array of char);[Public,Alias:'FPC_WRITE_TEXT_PCHAR_AS_ARRAY'];
var
  ArrayLen : longint;
  p : pchar;
Begin
  If (InOutRes<>0) then
   exit;
  case f.mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        p:=pchar(@s);
        ArrayLen:=StrLen(p);
        if ArrayLen>sizeof(s) then
          ArrayLen:=sizeof(s);
        If Len>ArrayLen Then
          WriteBlanks(f,Len-ArrayLen);
        WriteBuffer(f,p^,ArrayLen);
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
End;


Procedure Write_PChar(Len : Longint;var f : TextRec;p : PChar);[Public,Alias:'FPC_WRITE_TEXT_PCHAR_AS_POINTER'];
var
  PCharLen : longint;
Begin
  If (p=nil) or (InOutRes<>0) then
   exit;
  case f.mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        PCharLen:=StrLen(p);
        If Len>PCharLen Then
          WriteBlanks(f,Len-PCharLen);
        WriteBuffer(f,p^,PCharLen);
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
End;


Procedure Write_Text_AnsiString (Len : Longint; Var T : TextRec; S : Pointer);[Public,alias:'FPC_WRITE_TEXT_ANSISTR'];
{
 Writes a AnsiString to the Text file T
}
begin
  If S=Nil then
   exit;
  Write_pchar (Len,t,PChar(S));
end;


Procedure Write_SInt(Len : Longint;var t : TextRec;l : ValSInt);[Public,Alias:'FPC_WRITE_TEXT_SINT'];
var
  s : String;
Begin
  If (InOutRes<>0) then
   exit;
  Str(l,s);
  Write_Str(Len,t,s);
End;


Procedure Write_UInt(Len : Longint;var t : TextRec;l : ValUInt);[Public,Alias:'FPC_WRITE_TEXT_UINT'];
var
  s : String;
Begin
  If (InOutRes<>0) then
   exit;
  Str(L,s);
  Write_Str(Len,t,s);
End;


{$ifdef INT64}
procedure write_qword(len : longint;var t : textrec;q : qword);[public,alias:'FPC_WRITE_TEXT_QWORD'];
var
  s : string;
begin
  if (InOutRes<>0) then
   exit;
  qword_str(q,s);
  write_str(len,t,s);
end;

procedure write_int64(len : longint;var t : textrec;i : int64);[public,alias:'FPC_WRITE_TEXT_INT64'];
var
  s : string;
begin
  if (InOutRes<>0) then
   exit;
  int64_str(i,s);
  write_str(len,t,s);
end;
{$endif INT64}


Procedure Write_Float(rt,fixkomma,Len : Longint;var t : TextRec;r : ValReal);[Public,Alias:'FPC_WRITE_TEXT_FLOAT'];
var
  s : String;
Begin
  If (InOutRes<>0) then
   exit;
  Str_real(Len,fixkomma,r,treal_type(rt),s);
  Write_Str(Len,t,s);
End;


Procedure Write_Boolean(Len : Longint;var t : TextRec;b : Boolean);[Public,Alias:'FPC_WRITE_TEXT_BOOLEAN'];
Begin
  If (InOutRes<>0) then
   exit;
{ Can't use array[boolean] because b can be >0 ! }
  if b then
    Write_Str(Len,t,'TRUE')
  else
    Write_Str(Len,t,'FALSE');
End;


Procedure Write_Char(Len : Longint;var t : TextRec;c : Char);[Public,Alias:'FPC_WRITE_TEXT_CHAR'];
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
   WriteBlanks(t,Len-1);
  If t.BufPos+1>=t.BufSize Then
   FileFunc(t.InOutFunc)(t);
  t.Bufptr^[t.BufPos]:=c;
  Inc(t.BufPos);
End;


{*****************************************************************************
                                Read(Ln)
*****************************************************************************}

Function NextChar(var f:TextRec;var s:string):Boolean;
begin
  if f.BufPos<f.BufEnd then
   begin
     if length(s)<high(s) then
      begin
        inc(s[0]);
        s[length(s)]:=f.BufPtr^[f.BufPos];
      end;
     Inc(f.BufPos);
     If f.BufPos>=f.BufEnd Then
      FileFunc(f.InOutFunc)(f);
     NextChar:=true;
   end
  else
   NextChar:=false;
end;


Function IgnoreSpaces(var f:TextRec):Boolean;
{
  Removes all leading spaces,tab,eols from the input buffer, returns true if
  the buffer is empty
}
var
  s : string;
begin
  s:='';
  IgnoreSpaces:=false;
  while f.Bufptr^[f.BufPos] in [#9,#10,#13,' '] do
   if not NextChar(f,s) then
    exit;
  IgnoreSpaces:=true;
end;


procedure ReadNumeric(var f:TextRec;var s:string);
{
  Read numeric input, if buffer is empty then return True
}
begin
  repeat
    if not NextChar(f,s) then
      exit;
  until (length(s)=high(s)) or (f.BufPtr^[f.BufPos] in [#9,#10,#13,' ']);
end;


Procedure Read_End(var f:TextRec);[Public,Alias:'FPC_READ_END'];
begin
  if f.FlushFunc<>nil then
   FileFunc(f.FlushFunc)(f);
end;


Procedure ReadLn_End(var f : TextRec);[Public,Alias:'FPC_READLN_END'];
var prev: char;
Begin
{ Check error and if file is open and load buf if empty }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
   begin
     case TextRec(f).mode of
      fmOutPut,fmAppend:
        InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  if f.BufPos>=f.BufEnd Then
   begin
     FileFunc(f.InOutFunc)(f);
     if (f.BufPos>=f.BufEnd) then
       { Flush if set }
       begin
         if (f.FlushFunc<>nil) then
           FileFunc(f.FlushFunc)(f);
         exit;
       end;
   end;
  repeat
    prev := f.BufPtr^[f.BufPos];
    inc(f.BufPos);
{ no system uses #10#13 as line seperator (#10 = *nix, #13 = Mac, }
{ #13#10 = Dos), so if we've got #10, we can safely exit          }
    if prev = #10 then
      exit;
    if f.BufPos>=f.BufEnd Then
      begin
        FileFunc(f.InOutFunc)(f);
        if (f.BufPos>=f.BufEnd) then
          { Flush if set }
          begin
           if (f.FlushFunc<>nil) then
             FileFunc(f.FlushFunc)(f);
           exit;
         end;
      end;
   if (prev=#13) then
     { is there also a #10 after it? }
     begin
       if (f.BufPtr^[f.BufPos]=#10) then
         { yes, skip that one as well }
         inc(f.BufPos);
       exit;
     end;
  until false;
End;


Function ReadPCharLen(var f:TextRec;s:pchar;maxlen:longint):longint;
var
  sPos,len : Longint;
  p,startp,maxp : pchar;
Begin
  ReadPCharLen:=0;
{ Check error and if file is open }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
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
    If f.BufPos>=f.BufEnd Then
     begin
       FileFunc(f.InOutFunc)(f);
       If f.BufPos>=f.BufEnd Then
         break;
     end;
    p:=@f.Bufptr^[f.BufPos];
    if SPos+f.BufEnd-f.BufPos>MaxLen then
     maxp:=@f.BufPtr^[f.BufPos+MaxLen-SPos]
    else
     maxp:=@f.Bufptr^[f.BufEnd];
    startp:=p;
  { search linefeed }
    while (p<maxp) and not(P^ in [#10,#13]) do
     inc(p);
  { calculate read bytes }
    len:=p-startp;
    inc(f.BufPos,Len);
    Move(startp^,s[sPos],Len);
    inc(sPos,Len);
  { was it a LF or CR? then leave }
    if (spos=MaxLen) or
       ((p<maxp) and (p^ in [#10,#13])) then
      break;
  until false;
  ReadPCharLen:=spos;
End;


Procedure Read_String(var f : TextRec;var s : String);[Public,Alias:'FPC_READ_TEXT_SHORTSTR'];
Begin
  s[0]:=chr(ReadPCharLen(f,pchar(@s[1]),high(s)));
End;


Procedure Read_PChar(var f : TextRec;var s : PChar);[Public,Alias:'FPC_READ_TEXT_PCHAR_AS_POINTER'];
Begin
  pchar(s+ReadPCharLen(f,s,$7fffffff))^:=#0;
End;


Procedure Read_Array(var f : TextRec;var s : array of char);[Public,Alias:'FPC_READ_TEXT_PCHAR_AS_ARRAY'];
Begin
  pchar(pchar(@s)+ReadPCharLen(f,pchar(@s),high(s)))^:=#0;
End;


Procedure Read_AnsiString(var f : TextRec;var s : AnsiString);[Public,Alias:'FPC_READ_TEXT_ANSISTR'];
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


Function Read_Char(var f : TextRec):char;[Public,Alias:'FPC_READ_TEXT_CHAR'];
Begin
  Read_Char:=#0;
{ Check error and if file is open }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
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
  If f.BufPos>=f.BufEnd Then
   begin
     FileFunc(f.InOutFunc)(f);
     If f.BufPos>=f.BufEnd Then
       exit(#26);
   end;
  Read_Char:=f.Bufptr^[f.BufPos];
  inc(f.BufPos);
end;


Function Read_SInt(var f : TextRec):ValSInt;[Public,Alias:'FPC_READ_TEXT_SINT'];
var
  hs   : String;
  code : Longint;
Begin
  Read_SInt:=0;
{ Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  If f.BufPos>=f.BufEnd Then
   FileFunc(f.InOutFunc)(f);
  hs:='';
  if IgnoreSpaces(f) then
   ReadNumeric(f,hs);
  Val(hs,Read_SInt,code);
  If code<>0 Then
   InOutRes:=106;
End;


Function Read_UInt(var f : TextRec):ValUInt;[Public,Alias:'FPC_READ_TEXT_UINT'];
var
  hs   : String;
  code : longint;
Begin
  Read_UInt:=0;
{ Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  If f.BufPos>=f.BufEnd Then
   FileFunc(f.InOutFunc)(f);
  hs:='';
  if IgnoreSpaces(f) then
   ReadNumeric(f,hs);
  val(hs,Read_UInt,code);
  If code<>0 Then
   InOutRes:=106;
End;


Function Read_Float(var f : TextRec):ValReal;[Public,Alias:'FPC_READ_TEXT_FLOAT'];
var
  hs : string;
  code : Word;
begin
  Read_Float:=0.0;
{ Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  If f.BufPos>=f.BufEnd Then
   FileFunc(f.InOutFunc)(f);
  hs:='';
  if IgnoreSpaces(f) then
    ReadNumeric(f,hs);
  val(hs,Read_Float,code);
  If code<>0 Then
   InOutRes:=106;
end;


{$ifdef INT64}
function Read_QWord(var f : textrec) : qword;[public,alias:'FPC_READ_TEXT_QWORD'];
var
  hs   : String;
  code : longint;
Begin
  Read_QWord:=0;
  { Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  If f.BufPos>=f.BufEnd Then
   FileFunc(f.InOutFunc)(f);
  hs:='';
  if IgnoreSpaces(f) then
   ReadNumeric(f,hs);
  val(hs,Read_QWord,code);
  If code<>0 Then
   InOutRes:=106;
End;

function Read_Int64(var f : textrec) : int64;[public,alias:'FPC_READ_TEXT_INT64'];
var
  hs   : String;
  code : Longint;
Begin
  Read_Int64:=0;
{ Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
   begin
     case TextRec(f).mode of
       fmOutPut,fmAppend:
         InOutRes:=104
       else
         InOutRes:=103;
     end;
     exit;
   end;
  If f.BufPos>=f.BufEnd Then
   FileFunc(f.InOutFunc)(f);
  hs:='';
  if IgnoreSpaces(f) then
   ReadNumeric(f,hs);
  Val(hs,Read_Int64,code);
  If code<>0 Then
   InOutRes:=106;
End;
{$endif INT64}


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


{
  $Log$
  Revision 1.1  2000-07-13 06:30:49  michael
  + Initial import

  Revision 1.72  2000/03/24 10:26:18  jonas
    * changed a lot of "if fm.mode = fmClosed then" to case statements,
      because if f is not yet initialized, the mode is invalid and can
      contain another value even though the file is closed
    + check if a file is open in writeln_end (caused crash if used on
      not opened files)

  Revision 1.71  2000/03/19 08:36:41  peter
    * length check for readnumeric

  Revision 1.70  2000/03/17 21:27:56  jonas
    * fixed declaration of val_int64 (removed destsize parameter)
    * fixed val_int64 and val_qword so they reject invalid input
      (u >= base)
    * when reading a number, invalid input is removed from the input
      buffer (+ it should be faster as well)

  Revision 1.69  2000/02/09 16:59:31  peter
    * truncated log

  Revision 1.68  2000/01/31 12:11:53  jonas
    * committed the rest of my fix :)

  Revision 1.67  2000/01/31 10:15:43  pierre
   * Jonas' fix for bug811

  Revision 1.66  2000/01/23 12:22:37  florian
    * reading of 64 bit type implemented

  Revision 1.65  2000/01/20 20:19:37  florian
   * writing of int64/qword fixed

  Revision 1.64  2000/01/08 17:08:36  jonas
    + Mac linebreak (#13) support for readln

  Revision 1.63  2000/01/07 16:41:36  daniel
    * copyright 2000

  Revision 1.62  2000/01/07 16:32:25  daniel
    * copyright 2000 added

  Revision 1.61  1999/12/02 17:40:06  peter
    * read_int64 dummy added

  Revision 1.60  1999/11/06 14:35:39  peter
    * truncated log

  Revision 1.59  1999/10/26 12:25:19  peter
    * inoutres 103 for closed files, just like delphi

  Revision 1.58  1999/10/04 20:42:45  peter
    * read ansistring speedup (no length(s) calls anymore)

  Revision 1.57  1999/09/10 17:14:43  peter
    * remove CR when reading one char less then size

  Revision 1.56  1999/09/10 15:40:33  peter
    * fixed do_open flags to be > $100, becuase filemode can be upto 255

  Revision 1.55  1999/09/08 16:12:24  peter
    * fixed inoutres for diskfull

  Revision 1.54  1999/09/07 07:44:58  peter
    * fixed array of char writing which didn't write the last char

  Revision 1.53  1999/08/19 11:16:14  peter
    * settextbuf size is now longint

  Revision 1.52  1999/08/03 21:58:45  peter
    * small speed improvements

  Revision 1.51  1999/07/26 09:43:24  florian
    + write helper routine for in64 implemented

}
