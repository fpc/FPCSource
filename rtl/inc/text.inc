{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

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
Begin
  Do_Write(t.Handle,Longint(t.Bufptr),t.BufPos);
  t.BufPos:=0;
End;


Procedure FileOpenFunc(var t:TextRec);
var
  Flags : Longint;
Begin
  Case t.mode Of
    fmInput : Flags:=$1000;
   fmOutput : Flags:=$1101;
   fmAppend : Flags:=$1011;
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
   { Only install flushing if its a NOT a file }
     if Do_Isdevice(t.Handle) then
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
  If (TextRec(t).mode<>fmClosed) Then
   Begin
   { Write pending buffer }
     If Textrec(t).Mode=fmoutput then
       FileFunc(TextRec(t).InOutFunc)(TextRec(t));
     TextRec(t).mode:=fmClosed;
   { Only close functions not connected to stdout.}
     If ((TextRec(t).Handle<>StdInputHandle) and
         (TextRec(t).Handle<>StdOutputHandle) and
         (TextRec(t).Handle<>StdErrorHandle)) Then
      FileFunc(TextRec(t).CloseFunc)(TextRec(t));
   { Reset buffer for safety }
     TextRec(t).BufPos:=0;
     TextRec(t).BufEnd:=0;
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
  If TextRec(t).mode<>fmOutput Then
   begin
     InOutres:=105;
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
     InOutRes:=104;
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
     InOutRes:=104;
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
     InOutRes:=104;
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
     InOutRes:=104;
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
{ Write EOL }
  WriteBuffer(f,eol,eollen);
{ Flush }
  if f.FlushFunc<>nil then
   FileFunc(f.FlushFunc)(f);
end;


Procedure Write_Str(Len : Longint;var f : TextRec;const s : String);[Public,Alias:'FPC_WRITE_TEXT_SHORTSTR'];
Begin
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmOutput) Then
   begin
     InOutRes:=105;
     exit;
   end;
  If Len>Length(s) Then
   WriteBlanks(f,Len-Length(s));
  WriteBuffer(f,s[1],Length(s));
End;


Procedure Write_Array(Len : Longint;var f : TextRec;const s : array of char);[Public,Alias:'FPC_WRITE_TEXT_PCHAR_AS_ARRAY'];
var
  ArrayLen : longint;
  p : pchar;
Begin
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmOutput) Then
   begin
     InOutRes:=105;
     exit;
   end;
  p:=pchar(@s);
  ArrayLen:=StrLen(p);
  if ArrayLen>high(s) then
   ArrayLen:=high(s);
  If Len>ArrayLen Then
   WriteBlanks(f,Len-ArrayLen);
  WriteBuffer(f,p^,ArrayLen);
End;


Procedure Write_PChar(Len : Longint;var f : TextRec;p : PChar);[Public,Alias:'FPC_WRITE_TEXT_PCHAR_AS_POINTER'];
var
  PCharLen : longint;
Begin
  If (p=nil) or (InOutRes<>0) then
   exit;
  if (f.mode<>fmOutput) Then
   begin
     InOutRes:=105;
     exit;
   end;
  PCharLen:=StrLen(p);
  If Len>PCharLen Then
   WriteBlanks(f,Len-PCharLen);
  WriteBuffer(f,p^,PCharLen);
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
  int_str(q,s);
  write_str(len,t,s);
end;

procedure write_int64(len : longint;var t : textrec;i : int64);[public,alias:'FPC_WRITE_TEXT_INT64'];
var
  s : string;
begin
  if (InOutRes<>0) then
   exit;
  int_str(i,s);
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


Function ReadSign(var f:TextRec;var s:string):Boolean;
{
  Read + and - sign, return true if buffer is empty
}
begin
  ReadSign:=(not (f.Bufptr^[f.BufPos] in ['-','+'])) or NextChar(f,s);
end;


Function ReadBase(var f:TextRec;var s:string;var Base:longint):boolean;
{
  Read the base $ For 16 and % For 2, if buffer is empty return true
}
begin
  case f.BufPtr^[f.BufPos] of
   '$' : Base:=16;
   '%' : Base:=2;
  else
   Base:=10;
  end;
  ReadBase:=(Base=10) or NextChar(f,s);
end;


Function ReadNumeric(var f:TextRec;var s:string;base:longint):Boolean;
{
  Read numeric input, if buffer is empty then return True
}
var
  c : char;
begin
  ReadNumeric:=false;
  c:=f.BufPtr^[f.BufPos];
  while ((base>=10) and (c in ['0'..'9'])) or
        ((base=16) and (c in ['A'..'F','a'..'f'])) or
        ((base=2) and (c in ['0'..'1'])) do
   begin
     if not NextChar(f,s) then
      exit;
     c:=f.BufPtr^[f.BufPos];
   end;
  ReadNumeric:=true;
end;


Procedure Read_End(var f:TextRec);[Public,Alias:'FPC_READ_END'];
begin
  if f.FlushFunc<>nil then
   FileFunc(f.FlushFunc)(f);
end;


Procedure ReadLn_End(var f : TextRec);[Public,Alias:'FPC_READLN_END'];
Begin
{ Check error and if file is open and load buf if empty }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
   begin
     InOutRes:=104;
     exit;
   end;
  repeat
    If f.BufPos>=f.BufEnd Then
     begin
       FileFunc(f.InOutFunc)(f);
       if f.BufPos>=f.BufEnd then
        break;
     end;
    inc(f.BufPos);
    if (f.BufPtr^[f.BufPos-1]=#10) then
     exit;
  until false;
{ Flush if set }
  if f.FlushFunc<>nil then
   FileFunc(f.FlushFunc)(f);
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
     InOutRes:=104;
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
    while (p<maxp) and (P^<>#10) do
     inc(p);
  { calculate read bytes }
    len:=p-startp;
    inc(f.BufPos,Len);
    Move(startp^,s[sPos],Len);
    inc(sPos,Len);
  { was it a LF? then leave }
    if (p<maxp) and (p^=#10) then
     begin
       if (spos>0) and (s[spos-1]=#13) then
        dec(sPos);
       break;
     end;
  { Maxlen reached ? }
    if spos=MaxLen then
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
  len : longint;
Begin
{ Delete the string }
  Setlength(S,0);
  Repeat
    // SetLength will reallocate the length.
    SetLength(S,Length(S)+255);
    len:=ReadPCharLen(f,pchar(Pointer(S)+Length(S)-255),255);
    If Len<255 then
      // Set actual length
      SetLength(S,Length(S)-255+Len);
  Until len<255;
End;


Function Read_Char(var f : TextRec):char;[Public,Alias:'FPC_READ_TEXT_CHAR'];
Begin
  Read_Char:=#0;
{ Check error and if file is open }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
   begin
     InOutRes:=104;
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
  base : longint;
Begin
  Read_SInt:=0;
{ Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
   begin
     InOutRes:=104;
     exit;
   end;
  If f.BufPos>=f.BufEnd Then
   FileFunc(f.InOutFunc)(f);
  hs:='';
  if IgnoreSpaces(f) and ReadSign(f,hs) and ReadBase(f,hs,Base) then
   ReadNumeric(f,hs,Base);
  Val(hs,Read_SInt,code);
  If code<>0 Then
   InOutRes:=106;
End;


Function Read_UInt(var f : TextRec):ValUInt;[Public,Alias:'FPC_READ_TEXT_UINT'];
var
  hs   : String;
  code : longint;
  base : longint;
Begin
  Read_UInt:=0;
{ Leave if error or not open file, else check for empty buf }
  If (InOutRes<>0) then
   exit;
  if (f.mode<>fmInput) Then
   begin
     InOutRes:=104;
     exit;
   end;
  If f.BufPos>=f.BufEnd Then
   FileFunc(f.InOutFunc)(f);
  hs:='';
  if IgnoreSpaces(f) and ReadSign(f,hs) and ReadBase(f,hs,Base) then
   ReadNumeric(f,hs,Base);
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
     InOutRes:=104;
     exit;
   end;
  If f.BufPos>=f.BufEnd Then
   FileFunc(f.InOutFunc)(f);
  hs:='';
  if IgnoreSpaces(f) and ReadSign(f,hs) and ReadNumeric(f,hs,10) then
   begin
   { First check for a . }
     if (f.Bufptr^[f.BufPos]='.') and (f.BufPos<f.BufEnd) Then
      begin
        hs:=hs+'.';
        Inc(f.BufPos);
        If f.BufPos>=f.BufEnd Then
         FileFunc(f.InOutFunc)(f);
        ReadNumeric(f,hs,10);
      end;
   { Also when a point is found check for a E }
     if (f.Bufptr^[f.BufPos] in ['e','E']) and (f.BufPos<f.BufEnd) Then
      begin
        hs:=hs+'E';
        Inc(f.BufPos);
        If f.BufPos>=f.BufEnd Then
         FileFunc(f.InOutFunc)(f);
        if ReadSign(f,hs) then
         ReadNumeric(f,hs,10);
      end;
   end;
  val(hs,Read_Float,code);
  If code<>0 Then
   InOutRes:=106;
end;


{$ifdef INT64}
procedure read_qword(len : longint;var t : textrec;q : qword);[public,alias:'FPC_READ_TEXT_QWORD'];
begin
  { !!!!!!!!!!!!! }
end;
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
  Revision 1.53  1999-08-19 11:16:14  peter
    * settextbuf size is now longint

  Revision 1.52  1999/08/03 21:58:45  peter
    * small speed improvements

  Revision 1.51  1999/07/26 09:43:24  florian
    + write helper routine for in64 implemented

  Revision 1.50  1999/07/08 15:18:14  michael
  * Now ansistring of arbitrary length can be read

  Revision 1.49  1999/07/05 20:04:29  peter
    * removed temp defines

  Revision 1.48  1999/07/01 15:39:52  florian
    + qword/int64 type released

  Revision 1.47  1999/06/30 22:17:24  florian
    + fpuint64 to system unit interface added: if it is true, the rtl
      uses the fpu to do int64 operations, if possible

  Revision 1.46  1999/05/06 09:05:16  peter
    * generic write_float str_float

  Revision 1.45  1999/04/26 18:27:26  peter
    * fixed write array
    * read array with maxlen

  Revision 1.44  1999/04/08 15:57:57  peter
    + subrange checking for readln()

  Revision 1.43  1999/04/07 22:05:18  peter
    * fixed bug with readln where it sometime didn't read until eol

  Revision 1.42  1999/03/16 17:49:39  jonas
    * changes for internal Val code (do a "make cycle OPT=-dvalintern" to test)
    * in text.inc: changed RTE 106 when read integer values are out of bounds to RTE 201
    * in systemh.inc: disabled "support_fixed" for the i386 because it gave internal errors,

  Revision 1.41  1999/03/02 18:23:37  peter
    * changed so handlerror() -> inoutres:= to have $I- support

  Revision 1.40  1999/03/01 15:41:04  peter
    * use external names
    * removed all direct assembler modes

  Revision 1.39  1999/02/17 10:13:29  peter
    * when error when opening a file, then reset the mode to fmclosed

  Revision 1.38  1999/01/28 19:38:19  peter
    * fixed readln(ansistring)

  Revision 1.37  1998/12/15 22:43:06  peter
    * removed temp symbols

  Revision 1.36  1998/12/11 18:07:39  peter
    * fixed read(char) with empty buffer

  Revision 1.35  1998/11/27 14:50:58  peter
    + open strings, $P switch support

  Revision 1.34  1998/11/16 12:21:48  peter
    * fixes for 0.99.8

  Revision 1.33  1998/10/23 00:03:29  peter
    * write(pchar) has check for nil

  Revision 1.32  1998/10/20 14:37:45  peter
    * fixed maxlen which was not correct after my read_string update

  Revision 1.31  1998/10/10 15:28:48  peter
    + read single,fixed
    + val with code:longint
    + val for fixed

  Revision 1.30  1998/09/29 08:39:07  michael
  + Ansistring write now gets pointer.

  Revision 1.29  1998/09/28 14:27:08  michael
  + AnsiStrings update

  Revision 1.28  1998/09/24 23:32:24  peter
    * fixed small bug with a #13#10 on a line

  Revision 1.27  1998/09/18 12:23:22  peter
    * fixed a bug introduced by my previous update

  Revision 1.26  1998/09/17 16:34:18  peter
    * new eof,eoln,seekeoln,seekeof
    * speed upgrade for read_string
    * inoutres 104/105 updates for read_* and write_*

  Revision 1.25  1998/09/14 10:48:23  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.24  1998/09/08 10:14:06  peter
    + textrecbufsize

  Revision 1.23  1998/08/26 15:33:28  peter
    * reset bufpos,bufend in opentext like tp7

  Revision 1.22  1998/08/26 11:23:25  pierre
    * close did not reset the bufpos and bufend fields
      led to problems when using the same file several times

  Revision 1.21  1998/08/17 22:42:17  michael
  + Flush on close only for output files cd ../inc

  Revision 1.20  1998/08/11 00:05:28  peter
    * $ifdef ver0_99_5 updates

  Revision 1.19  1998/07/30 13:26:16  michael
  + Added support for ErrorProc variable. All internal functions are required
    to call HandleError instead of runerror from now on.
    This is necessary for exception support.

  Revision 1.18  1998/07/29 21:44:35  michael
  + Implemented reading/writing of ansistrings

  Revision 1.17  1998/07/19 19:55:33  michael
  + fixed rename. Changed p to p^

  Revision 1.16  1998/07/10 11:02:40  peter
    * support_fixed, becuase fixed is not 100% yet for the m68k

  Revision 1.15  1998/07/06 15:56:43  michael
  Added length checking for string reading

  Revision 1.14  1998/07/02 12:14:56  carl
    + Each IOCheck routine now check InOutRes before, just like TP

  Revision 1.13  1998/07/01 15:30:00  peter
    * better readln/writeln

  Revision 1.12  1998/07/01 14:48:10  carl
    * bugfix of WRITE_TEXT_BOOLEAN , was not TP compatible
    + added explicit typecast in OpenText

  Revision 1.11  1998/06/25 09:44:22  daniel
  + RTLLITE directive to compile minimal RTL.

  Revision 1.10  1998/06/04 23:46:03  peter
    * comp,extended are only i386 added support_comp,support_extended

  Revision 1.9  1998/06/02 16:47:56  pierre
    * bug for boolean values greater than one fixed

  Revision 1.8  1998/05/31 14:14:54  peter
    * removed warnings using comp()

  Revision 1.7  1998/05/27 00:19:21  peter
    * fixed crt input

  Revision 1.6  1998/05/21 19:31:01  peter
    * objects compiles for linux
    + assign(pchar), assign(char), rename(pchar), rename(char)
    * fixed read_text_as_array
    + read_text_as_pchar which was not yet in the rtl

  Revision 1.5  1998/05/12 10:42:45  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

  Revision 1.4  1998/04/07 22:40:46  florian
    * final fix of comp writing
}
