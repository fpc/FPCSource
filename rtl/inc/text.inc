{    $Id$
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

  EXTENDED_EOF    Use extended EOF checking for textfile, necessary for
                  Pipes and Sockets under Linux
  EOF_CTRLZ       Is Ctrl-Z (#26) a EOF mark for textfiles
  SHORT_LINEBREAK Use short Linebreaks #10 instead of #10#13

  Both EXTENDED_EOF and SHORT_LINEBREAK are defined in the Linux system
  unit (syslinux.pp)
}

{****************************************************************************
                    subroutines For TextFile handling
****************************************************************************}


Procedure FileCloseFunc(Var t:TextRec);
Begin
  Do_Close(t.Handle);
  t.Handle:=UnusedHandle;
End;


Procedure FileInOutFunc(var t:TextRec);
Begin
  Case t.mode Of
   fmoutput : Do_Write(t.Handle,Longint(t.Bufptr),t.BufPos);
    fminput : t.BufEnd:=Do_Read(t.Handle,Longint(t.Bufptr),t.BufSize);
  else
   RunError(102);
  End;
  t.BufPos:=0;
End;


Procedure FileOpenFunc(var t:TextRec);
var
  Flags : Longint;
Begin
  t.InOutFunc:=@FileInOutFunc;
  t.FlushFunc:=@FileInOutFunc;
  t.CloseFunc:=@FileCloseFunc;
  Case t.mode Of
    fmInput : Flags:=$1000;
   fmOutput : Flags:=$1101;
   fmAppend : Flags:=$1011;
  End;
  Do_Open(t,PChar(@TextRec(t).Name),Flags);
End;


Procedure assign(var t:Text;const s:String);
Begin
  FillChar(t,SizEof(TextRec),0);
  TextRec(t).Handle:=UnusedHandle;
  TextRec(t).mode:=fmClosed;
  TextRec(t).BufSize:=128;
  TextRec(t).Bufpos:=0;
  TextRec(T).Bufend:=0;
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


Procedure Close(var t : Text);[Public,Alias: 'CLOSE_TEXT',IOCheck];
Begin
  If (TextRec(t).mode<>fmClosed) Then
   Begin
     FileFunc(TextRec(t).FlushFunc)(TextRec(t));
     TextRec(t).mode:=fmClosed;
     { Only close functions not connected to stdout.}
     If ((TextRec(t).Handle<>StdInputHandle) or
         (TextRec(t).Handle<>StdOutputHandle) or
         (TextRec(t).Handle<>StdErrorHandle)) Then
      FileFunc(TextRec(t).CloseFunc)(TextRec(t));
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
  If TextRec(t).Name[0]<>#0 Then
   FileFunc(TextRec(t).OpenFunc)(TextRec(t))
  else
   Begin
     TextRec(t).Handle:=defHdl;
     TextRec(t).InOutFunc:=@FileInOutFunc;
     TextRec(t).FlushFunc:=@FileInOutFunc;
     TextRec(t).CloseFunc:=@FileCloseFunc;
   End;
End;


Procedure Rewrite(var t : Text);[IOCheck];
Begin
  OpenText(t,fmOutput,1);
End;


Procedure Reset(var t : Text);[IOCheck];
Begin
  OpenText(t,fmInput,0);
End;


Procedure Append(var t : Text);[IOCheck];
Begin
  OpenText(t,fmAppend,1);
End;


Procedure Flush(var t : Text);[IOCheck];
Begin
  If TextRec(t).mode<>fmOutput Then
   exit;
  FileFunc(TextRec(t).FlushFunc)(TextRec(t));
End;


Procedure Erase(var t:Text);[IOCheck];
Begin
  If TextRec(t).mode=fmClosed Then
   Do_Erase(PChar(@TextRec(t).Name));
End;


Procedure Rename(var t : text;p:pchar);[IOCheck];
Begin
  If TextRec(t).mode=fmClosed Then
   Begin
     Do_Rename(PChar(@TextRec(t).Name),p);
     Move(p,TextRec(t).Name,StrLen(p)+1);
   End;
End;


Procedure Rename(var t : Text;const s : string);[IOCheck];
var
  p : array[0..255] Of Char;
Begin
  Move(s[1],p,Length(s));
  p[Length(s)]:=#0;
  Rename(t,Pchar(@p));
End;


Procedure Rename(var t : Text;c : char);[IOCheck];
var
  p : array[0..1] Of Char;
Begin
  p[0]:=c;
  p[1]:=#0;
  Rename(t,Pchar(@p));
End;


Function Eof(Var t: Text): Boolean;[IOCheck];
Begin
{$IFNDEF EXTENDED_EOF}
  {$IFDEF EOF_CTRLZ}
    Eof:=TextRec(t).Buffer[TextRec(t).BufPos]=#26;
    If Eof Then
     Exit;
  {$ENDIF EOL_CTRLZ}
  Eof:=(Do_FileSize(TextRec(t).Handle)<=Do_FilePos(TextRec(t).Handle));
  If Eof Then
   Eof:=TextRec(t).BufEnd <= TextRec(t).BufPos;
{$ELSE EXTENDED_EOF}
  { The previous method will NOT work on stdin and pipes or sockets.
    So how to do it ?
     1) Check if characters in buffer - Yes ? Eof=false;
     2) Read buffer full. If 0 Chars Read : Eof !
    Michael.}
  If TextRec(T).mode=fmClosed Then  { Sanity Check }
   Begin
     Eof:=True;
     Exit;
   End;
  If (TextRec(T).BufPos < TextRec(T).BufEnd) Then
   Begin
     Eof:=False;
     Exit
   End;
  TextRec(T).BufPos:=0;
  TextRec(T).BufEnd:=Do_Read(TextRec(T).Handle,Longint(TextRec(T).BufPtr),TextRec(T).BufSize);
  If TextRec(T).BufEnd<0 Then
   TextRec(T).BufEnd:=0;
  Eof:=(TextRec(T).BufEnd=0);
{$ENDIF EXTENDED_EOF}
End;


Function Eof:Boolean;
Begin
  Eof:=Eof(Input);
End;


Function SeekEof (Var F : Text) : Boolean;
Var
  TR   : ^TextRec;
  Temp : Longint;
Begin
  TR:=@TextRec(f);
  If TR^.mode<>fmInput Then exit (true);
  SeekEof:=True;
  {No data in buffer ? Fill it }
  If TR^.BufPos>=TR^.BufEnd Then
   FileFunc(TR^.InOutFunc)(TR^);

  Temp:=TR^.BufPos;
  while (TR^.BufPos<TR^.BufEnd) Do
   Begin
     If (TR^.Bufptr^[Temp] In [#9,#10,#13,' ']) Then
      Inc(Temp)
     else
      Begin
        SeekEof:=False;
        TR^.BufPos:=Temp;
        exit;
      End;
     If Temp>=TR^.BufEnd Then
      Begin
        FileFunc(TR^.InOutFunc)(TR^);
        Temp:=TR^.BufPos+1;
      End;
   End;
End;


Function SeekEof : Boolean;
Begin
  SeekEof:=SeekEof(Input);
End;


Function Eoln(var t:Text) : Boolean;
Begin
{ maybe we need new data }
  If TextRec(t).BufPos>=TextRec(t).BufEnd Then
   FileFunc(TextRec(t).InOutFunc)(TextRec(t));
  Eoln:=Eof(t) or (TextRec(t).Bufptr^[TextRec(t).BufPos] In [#10,#13]);
End;


Function Eoln : Boolean;
Begin
  Eoln:=Eoln(Input);
End;


Function SeekEoln (Var F : Text) : Boolean;
Var
  TR   : ^TextRec;
  Temp : Longint;
Begin
  TR:=@TextRec(f);
  If TR^.mode<>fmInput Then
   exit (true);
  SeekEoln:=True;
  {No data in buffer ? Fill it }
  If TR^.BufPos>=TR^.BufEnd Then
   FileFunc(TR^.InOutFunc)(TR^);
  Temp:=TR^.BufPos;
  while (TR^.BufPos<TR^.BufEnd) Do
   Begin
     Case (TR^.Bufptr^[Temp]) Of
      #10 : Exit;
   #9,' ' : Inc(Temp)
     else
      Begin
        SeekEoln:=False;
        TR^.BufPos:=Temp;
        exit;
      End;
     End;
     If Temp>=TR^.BufEnd Then
      Begin
        FileFunc(TR^.InOutFunc)(TR^);
        Temp:=TR^.BufPos+1;
      End;
   End;
End;


Function SeekEoln : Boolean;
Begin
  SeekEoln:=SeekEoln(Input);
End;


Procedure SetTextBuf(Var F : Text; Var Buf);[INTERNPROC: In_settextbuf_file_x];


Procedure SetTextBuf(Var F : Text; Var Buf; Size : Word);
Begin
  TextRec(f).BufPtr:=@Buf;
  TextRec(f).BufSize:=Size;
  TextRec(f).BufPos:=0;
  TextRec(f).BufEnd:=0;
End;


{*****************************************************************************
                               Write(Ln)
*****************************************************************************}

Procedure w(Len : Longint;var f : TextRec;var s : String);[Public,Alias: 'WRITE_TEXT_STRING'];
var
  hbytes,Pos,copybytes : Longint;
  hs : String;
Begin
  If f.mode<>fmOutput Then
   exit;
  copybytes:=Length(s);
  If Len>copybytes Then
   Begin
     hs:=Space(Len-copybytes);
     w(0,f,hs);
   End;
  Pos:=1;
  hbytes:=f.BufSize-f.BufPos;
  { If no room in Buffer, do a flush. }
  If hbytes=0 Then
   FileFunc(f.FlushFunc)(f);
  while copybytes>hbytes Do
   Begin
     Move(s[Pos],f.Bufptr^[f.BufPos],hbytes);
     f.BufPos:=f.BufPos+hbytes;
     dec(copybytes,hbytes);
     Inc(Pos,hbytes);
     FileFunc(f.InOutFunc)(f);
     hbytes:=f.BufSize-f.BufPos;
   End;
  Move(s[Pos],f.Bufptr^[f.BufPos],copybytes);
  f.BufPos:=f.BufPos+copybytes;
End;


Procedure w(var t : TextRec);[Public,Alias: 'WRITELN_TEXT'];
var
  hs : String;
Begin
{$IFDEF SHORT_LINEBREAK}
  hs:=#10;
{$ELSE}
  hs:=#13#10;
{$ENDIF}
  w(0,t,hs);
End;


Type
   array00 = array[0..0] Of Char;
Procedure w(Len : Longint;var f : TextRec;const p : array00);[Public,Alias: 'WRITE_TEXT_PCHAR_AS_ARRAY'];
var
  hbytes,Pos,copybytes : Longint;
  hs : String;
Begin
  If f.mode<>fmOutput Then
   exit;
  copybytes:=StrLen(p);
  If Len>copybytes Then
   Begin
     hs:=Space(Len-copybytes);
     w(0,f,hs);
   End;
  Pos:=0;
  hbytes:=f.BufSize-f.BufPos;
  { If no room in buffer , do a flush. }
  If hbytes=0 Then
    FileFunc(f.FlushFunc)(f);
  while copybytes>hbytes Do
   Begin
     Move(p[Pos],f.Bufptr^[f.BufPos],hbytes);
     f.BufPos:=f.BufPos+hbytes;
     dec(copybytes,hbytes);
     Inc(Pos,hbytes);
     FileFunc(f.InOutFunc)(f);
     hbytes:=f.BufSize-f.BufPos;
   End;
  Move(p[Pos],f.Bufptr^[f.BufPos],copybytes);
  f.BufPos:=f.BufPos+copybytes;
End;


Procedure wa(Len : Longint;var f : TextRec;p : PChar);[Public,Alias: 'WRITE_TEXT_PCHAR_AS_POINTER'];
Begin
  w(Len,f,p);
End;


Procedure w(Len : Longint;var t : TextRec;l : Longint);[Public,Alias: 'WRITE_TEXT_LONGINT'];
var
  s : String;
Begin
  Str(l,s);
  w(Len,t,s);
End;


Procedure w(fixkomma,Len : Longint;var t : TextRec;r : real);[Public,Alias: 'WRITE_TEXT_REAL'];
var
   s : String;
Begin
{$ifdef i386}
   Str_real(Len,fixkomma,r,rt_s64real,s);
{$else}
   Str_real(Len,fixkomma,r,rt_s32real,s);
{$endif}
   w(Len,t,s);
End;


Procedure w(Len : Longint;var t : TextRec;l : cardinal);[Public,Alias: 'WRITE_TEXT_CARDINAL'];
var
  s : String;
Begin
  Str(L,s);
  w(Len,t,s);
End;


Procedure w(fixkomma,Len : Longint;var t : TextRec;r : single);[Public,Alias: 'WRITE_TEXT_SINGLE'];
var
  s : String;
Begin
  Str_real(Len,fixkomma,r,rt_s32real,s);
  w(Len,t,s);
End;


Procedure w(fixkomma,Len : Longint;var t : TextRec;r : extended);[Public,Alias: 'WRITE_TEXT_EXTENDED'];
var
  s : String;
Begin
  Str_real(Len,fixkomma,r,rt_s80real,s);
  w(Len,t,s);
End;


Procedure w(fixkomma,Len : Longint;var t : TextRec;r : comp);[Public,Alias: 'WRITE_TEXT_COMP'];
var
  s : String;
Begin
  Str_real(Len,fixkomma,r,rt_s64bit,s);
  w(Len,t,s);
End;


Procedure w(fixkomma,Len : Longint;var t : TextRec;r : fixed);[Public,Alias: 'WRITE_TEXT_FIXED'];
var
  s : String;
Begin
  Str_real(Len,fixkomma,r,rt_f32bit,s);
  w(Len,t,s);
End;


{ Is called wc to avoid recursive calling. }
Procedure wc(Len : Longint;var t : TextRec;b : Boolean);[Public,Alias: 'WRITE_TEXT_BOOLEAN'];
const
  BoolString:array[0..1] Of String[5]=('False','True');
Begin
   w(Len,t,String(BoolString[byte(b)]));
End;


Procedure wc(Len : Longint;var t : TextRec;c : Char);[Public,Alias: 'WRITE_TEXT_CHAR'];
var
   hs : String;
Begin
  If t.mode<>fmOutput Then
   exit;
  If Len>1 Then
   Begin
     hs:=Space(Len-1);
     w(0,t,hs);
   End;
  If t.BufPos+1>=t.BufSize Then
   FileFunc(t.FlushFunc)(t);
  t.Bufptr^[t.BufPos]:=c;
  Inc(t.BufPos);
End;


{*****************************************************************************
                                Read(Ln)
*****************************************************************************}

Function OpenInput(var f:TextRec):boolean;
begin
  If f.mode=fmInput Then
   begin
   { No characters in the buffer? Load them ! }
     If f.BufPos>=f.BufEnd Then
      FileFunc(f.InOutFunc)(f);
     OpenInput:=true;
   end
  else
   OpenInput:=false;
end;



Function NextChar(var f:TextRec;var s:string):Boolean;
begin
  if f.BufPos<f.BufEnd then
   begin
     s:=s+f.BufPtr^[f.BufPos];
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


Procedure r(var f : TextRec);[Public,Alias: 'READLN_TEXT'];
Begin
  if not OpenInput(f) then
   exit;
  while (f.BufPos<f.BufEnd) do
   begin
     inc(f.BufPos);
     if (f.BufPtr^[f.BufPos-1]=#10) then
      exit;
     If f.BufPos>=f.BufEnd Then
      FileFunc(f.InOutFunc)(f);
   end;
End;


Procedure r(var f : TextRec;var s : String);[Public,Alias: 'READ_TEXT_STRING'];
var
  Temp,sPos : Word;
Begin
  { Delete the string }
  s:='';
  if not OpenInput(f) then
   exit;
  Temp:=f.BufPos;
  sPos:=1;
  while (f.BufPos<f.BufEnd) and (f.Bufptr^[Temp]<>#10) Do
   Begin
   { search linefeed }
     while (f.Bufptr^[Temp]<>#10) and (Temp<f.BufEnd) Do
      Inc(Temp);
   { copy String. Take 255 char limit in account.}
     If sPos+Temp-f.BufPos<=255 Then
      Begin
        Move (f.Bufptr^[f.BufPos],s[sPos],Temp-f.BufPos);
        sPos:=sPos+Temp-f.BufPos;
        If s[sPos-1]=#13 Then
         dec(sPos);
      End
     else
      Begin
        If (sPos<=255) Then
         Move(f.Bufptr^[f.BufPos],s[sPos],256-sPos);
        sPos:=256
      End;
   { update f.BufPos }
     f.BufPos:=Temp;
     If Temp>=f.BufEnd Then
      Begin
        FileFunc(f.InOutFunc)(f);
        Temp:=f.BufPos;
      End
   End;
  s[0]:=chr(sPos-1);
End;


Procedure r(var f : TextRec;var c : Char);[Public,Alias: 'READ_TEXT_CHAR'];
Begin
  c:=#0;
  if not OpenInput(f) then
   exit;
  If f.BufPos>=f.BufEnd Then
   c:=#26
  else
   c:=f.Bufptr^[f.BufPos];
  Inc(f.BufPos);
End;


Procedure r(var f : TextRec;var s : PChar);[Public,Alias:'READ_TEXT_PCHAR_AS_POINTER'];
var
  p    : PChar;
  Temp : byte;
Begin
{ Delete the string }
  s^:=#0;
  p:=s;
  if not OpenInput(f) then
   exit;
  Temp:=f.BufPos;
  while (f.BufPos<f.BufEnd) and (f.Bufptr^[Temp]<>#10) Do
   Begin
     { search linefeed }
     while (f.Bufptr^[Temp]<>#10) and (Temp<f.BufEnd) Do
      inc(Temp);
     { copy string. }
     Move (f.Bufptr^[f.BufPos],p^,Temp-f.BufPos);
     Inc(Longint(p),Temp-f.BufPos);
     If pchar(p-1)^=#13 Then
      dec(p);
     { update f.BufPos }
     f.BufPos:=Temp;
     If Temp>=f.BufEnd Then
      Begin
        FileFunc(f.InOutFunc)(f);
        Temp:=f.BufPos;
      End
   End;
  p^:=#0;
End;


Procedure r(var f : TextRec;var s : array00);[Public,Alias:'READ_TEXT_PCHAR_AS_ARRAY'];
var
  p    : PChar;
  Temp : byte;
Begin
{ Delete the string }
  s[0]:=#0;
  p:=pchar(@s);
  if not OpenInput(f) then
   exit;
  Temp:=f.BufPos;
  while (f.BufPos<f.BufEnd) and (f.Bufptr^[Temp]<>#10) Do
   Begin
     { search linefeed }
     while (f.Bufptr^[Temp]<>#10) and (Temp<f.BufEnd) Do
      inc(Temp);
     { copy string. }
     Move (f.Bufptr^[f.BufPos],p^,Temp-f.BufPos);
     Inc(Longint(p),Temp-f.BufPos);
     If pchar(p-1)^=#13 Then
      dec(p);
     { update f.BufPos }
     f.BufPos:=Temp;
     If Temp>=f.BufEnd Then
      Begin
        FileFunc(f.InOutFunc)(f);
        Temp:=f.BufPos;
      End
   End;
  p^:=#0;
End;


Procedure r(var f : TextRec;var l : Longint);[Public,Alias: 'READ_TEXT_LONGINT'];
var
  hs   : String;
  code : Word;
  base : longint;
Begin
  l:=0;
  hs:='';
  if not OpenInput(f) then
   exit;
  if IgnoreSpaces(f) and ReadSign(f,hs) and ReadBase(f,hs,Base) then
   ReadNumeric(f,hs,Base);
  Val(hs,l,code);
  If code<>0 Then
   RunError(106);
End;


Procedure r(var f : TextRec;var l : Integer);[Public,Alias: 'READ_TEXT_INTEGER'];
var
  ll : Longint;
Begin
  r(f,ll);
  l:=0;
  If (ll<-32768) or (ll>32767) Then
   RunError(106);
  l:=ll;
End;


Procedure r(var f : TextRec;var l : Word);[Public,Alias: 'READ_TEXT_WORD'];
var
  ll : Longint;
Begin
  r(f,ll);
  l:=0;
  If (ll<0) or (ll>$ffff) Then
   RunError(106);
  l:=ll;
End;


Procedure r(var f : TextRec;var l : byte);[Public,Alias: 'READ_TEXT_BYTE'];
var
  ll : Longint;
Begin
  r(f,ll);
  l:=0;
  If (ll<0) or (ll>255) Then
   RunError(106);
  l:=ll;
End;


Procedure r(var f : TextRec;var l : shortint);[Public,Alias: 'READ_TEXT_SHORTINT'];
var
   ll : Longint;
Begin
  r(f,ll);
  l:=0;
  If (ll<-128) or (ll>127) Then
   RunError(106);
  l:=ll;
End;


Procedure r(var f : TextRec;var l : cardinal);[Public,Alias: 'READ_TEXT_CARDINAL'];
var
  hs   : String;
  code : Word;
  base : longint;
Begin
  l:=0;
  hs:='';
  if not OpenInput(f) then
   exit;
  if IgnoreSpaces(f) and ReadSign(f,hs) and ReadBase(f,hs,Base) then
   ReadNumeric(f,hs,Base);
  val(hs,l,code);
  If code<>0 Then
   RunError(106);
End;


Procedure r(var f : TextRec;var d : Real);[Public,Alias: 'READ_TEXT_REAL'];
var
  hs   : String;
  code : Word;
Begin
  d:=0.0;
  hs:='';
  if not OpenInput(f) then
   exit;
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
  val(hs,d,code);
  If code<>0 Then
   RunError(106);
End;


Procedure r(var f : TextRec;var d : extended);[Public,Alias: 'READ_TEXT_EXTENDED'];
var
  hs   : String;
  code : Word;
Begin
  d:=0.0;
  hs:='';
  if not OpenInput(f) then
   exit;
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
  val(hs,d,code);
  If code<>0 Then
   RunError(106);
End;


Procedure r(var f : TextRec;var d : comp);[Public,Alias: 'READ_TEXT_COMP'];
var
  hs   : String;
  code : Word;
Begin
  d:=0.0;
  hs:='';
  if not OpenInput(f) then
   exit;
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
  val(hs,d,code);
  If code<>0 Then
   RunError(106);
End;
{
  $Log$
  Revision 1.6  1998-05-21 19:31:01  peter
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
