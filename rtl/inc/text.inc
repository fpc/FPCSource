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
   HandleError(102);
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


Procedure Close(var t : Text);[Public,Alias: 'CLOSE_TEXT',IOCheck];
Begin
  if InOutRes <> 0 then Exit;
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
      { this was missing !!! PM }
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
  TextRec(t).mode:=word(mode);
  TextRec(t).bufpos:=0;
  TextRec(t).bufend:=0;
  FileFunc(TextRec(t).OpenFunc)(TextRec(t))
End;


Procedure Rewrite(var t : Text);[IOCheck];
Begin
  If InOutRes <> 0 then exit;
  OpenText(t,fmOutput,1);
End;


Procedure Reset(var t : Text);[IOCheck];
Begin
  If InOutRes <> 0 then exit;
  OpenText(t,fmInput,0);
End;


Procedure Append(var t : Text);[IOCheck];
Begin
  If InOutRes <> 0 then exit;
  OpenText(t,fmAppend,1);
End;


Procedure Flush(var t : Text);[IOCheck];
Begin
  If InOutRes <> 0 then exit;
  If TextRec(t).mode<>fmOutput Then
   exit;
{ Not the flushfunc but the inoutfunc should be used, becuase that
  writes the data, flushfunc doesn't need to be assigned }
  FileFunc(TextRec(t).InOutFunc)(TextRec(t));
End;


Procedure Erase(var t:Text);[IOCheck];
Begin
  If InOutRes <> 0 then exit;
  If TextRec(t).mode=fmClosed Then
   Do_Erase(PChar(@TextRec(t).Name));
End;


Procedure Rename(var t : text;p:pchar);[IOCheck];
Begin
  If InOutRes <> 0 then exit;
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
  If InOutRes <> 0 then exit;
  Move(s[1],p,Length(s));
  p[Length(s)]:=#0;
  Rename(t,Pchar(@p));
End;


Procedure Rename(var t : Text;c : char);[IOCheck];
var
  p : array[0..1] Of Char;
Begin
  If InOutRes <> 0 then exit;
  p[0]:=c;
  p[1]:=#0;
  Rename(t,Pchar(@p));
End;


Function Eof(Var t: Text): Boolean;[IOCheck];
Begin
  If InOutRes <> 0 then exit;
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


Procedure Write_End(var f:TextRec);[Public,Alias:'WRITE_END'];
begin
  if f.FlushFunc<>nil then
   FileFunc(f.FlushFunc)(f);
end;


Procedure Writeln_End(var f:TextRec);[Public,Alias:'WRITELN_END'];
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


Procedure Write_Str(Len : Longint;var f : TextRec;const s : String);[Public,Alias: 'WRITE_TEXT_STRING'];
Begin
  If InOutRes <> 0 then exit;
  If f.mode<>fmOutput Then
   exit;
  If Len>Length(s) Then
   WriteBlanks(f,Len-Length(s));
  WriteBuffer(f,s[1],Length(s));
End;


Type
   array00 = array[0..0] Of Char;
Procedure Write_Array(Len : Longint;var f : TextRec;const p : array00);[Public,Alias: 'WRITE_TEXT_PCHAR_AS_ARRAY'];
var
  ArrayLen : longint;
Begin
  If InOutRes <> 0 then exit;
  If f.mode<>fmOutput Then
   exit;
  ArrayLen:=StrLen(p);
  If Len>ArrayLen Then
   WriteBlanks(f,Len-ArrayLen);
  WriteBuffer(f,p,ArrayLen);
End;


Procedure Write_PChar(Len : Longint;var f : TextRec;p : PChar);[Public,Alias: 'WRITE_TEXT_PCHAR_AS_POINTER'];
var
  PCharLen : longint;
Begin
  If InOutRes <> 0 then exit;
  If f.mode<>fmOutput Then
   exit;
  PCharLen:=StrLen(p);
  If Len>PCharLen Then
   WriteBlanks(f,Len-PCharLen);
  WriteBuffer(f,p^,PCharLen);
End;

{$ifdef UseAnsiStrings}
Procedure Write_Text_AnsiString (Len : Longint; Var T : TextRec; Var S : AnsiString);[Public, alias: 'WRITE_TEXT_ANSISTRING'];
{
 Writes a AnsiString to the Text file T
}

Var Temp : Pointer;

begin
  Temp:=Pointer(S);
  If Temp=Nil then exit;
  Write_pchar (Len,t,PChar(Temp));
end;

{$endif}


Procedure Write_LongInt(Len : Longint;var t : TextRec;l : Longint);[Public,Alias: 'WRITE_TEXT_LONGINT'];
var
  s : String;
Begin
  If InOutRes <> 0 then exit;
  Str(l,s);
  Write_Str(Len,t,s);
End;


Procedure Write_Real(fixkomma,Len : Longint;var t : TextRec;r : real);[Public,Alias: 'WRITE_TEXT_REAL'];
var
   s : String;
Begin
  If InOutRes <> 0 then exit;
{$ifdef i386}
   Str_real(Len,fixkomma,r,rt_s64real,s);
{$else}
   Str_real(Len,fixkomma,r,rt_s32real,s);
{$endif}
   Write_Str(Len,t,s);
End;


Procedure Write_Cardinal(Len : Longint;var t : TextRec;l : cardinal);[Public,Alias: 'WRITE_TEXT_CARDINAL'];
var
  s : String;
Begin
  If InOutRes <> 0 then exit;
  Str(L,s);
  Write_Str(Len,t,s);
End;

{$ifdef SUPPORT_SINGLE}
Procedure Write_Single(fixkomma,Len : Longint;var t : TextRec;r : single);[Public,Alias: 'WRITE_TEXT_SINGLE'];
var
  s : String;
Begin
  If InOutRes <> 0 then exit;
  Str_real(Len,fixkomma,r,rt_s32real,s);
  Write_Str(Len,t,s);
End;
{$endif SUPPORT_SINGLE}


{$ifdef SUPPORT_EXTENDED}
Procedure Write_Extended(fixkomma,Len : Longint;var t : TextRec;r : extended);[Public,Alias: 'WRITE_TEXT_EXTENDED'];
var
  s : String;
Begin
  If InOutRes <> 0 then exit;
  Str_real(Len,fixkomma,r,rt_s80real,s);
  Write_Str(Len,t,s);
End;
{$endif SUPPORT_EXTENDED}


{$ifdef SUPPORT_COMP}
Procedure Write_Comp(fixkomma,Len : Longint;var t : TextRec;r : comp);[Public,Alias: 'WRITE_TEXT_COMP'];
var
  s : String;
Begin
  If InOutRes <> 0 then exit;
  Str_real(Len,fixkomma,r,rt_s64bit,s);
  Write_Str(Len,t,s);
End;
{$endif SUPPORT_COMP}


{$ifdef SUPPORT_FIXED}
Procedure Write_Fixed(fixkomma,Len : Longint;var t : TextRec;r : fixed);[Public,Alias: 'WRITE_TEXT_FIXED'];
var
  s : String;
Begin
  If InOutRes <> 0 then exit;
  Str_real(Len,fixkomma,r,rt_f32bit,s);
  Write_Str(Len,t,s);
End;
{$endif SUPPORT_FIXED}


Procedure Write_Boolean(Len : Longint;var t : TextRec;b : Boolean);[Public,Alias: 'WRITE_TEXT_BOOLEAN'];
Begin
  If InOutRes <> 0 then exit;
{ Can't use array[boolean] because b can be >0 ! }
  if b then
    Write_Str(Len,t,'TRUE')
  else
    Write_Str(Len,t,'FALSE');
End;


Procedure Write_Char(Len : Longint;var t : TextRec;c : Char);[Public,Alias: 'WRITE_TEXT_CHAR'];
Begin
  If InOutRes <> 0 then exit;
  If t.mode<>fmOutput Then
   exit;
  If Len>1 Then
   WriteBlanks(t,Len-1);
  If t.BufPos+1>=t.BufSize Then
   FileFunc(t.InOutFunc)(t);
  t.Bufptr^[t.BufPos]:=c;
  Inc(t.BufPos);
End;


{$ifdef VER0_99_5}
Procedure w(var t : TextRec);[Public,Alias: 'WRITELN_TEXT'];
var
  hs : String;
Begin
  If InOutRes <> 0 then exit;
  {$IFDEF SHORT_LINEBREAK}
   hs:=#10;
  {$ELSE}
   hs:=#13#10;
  {$ENDIF}
  Write_Str(0,t,hs);
End;
{$endif VER0_99_5}


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


Procedure Read_End(var f:TextRec);[Public,Alias:'READ_END'];
begin
  if f.FlushFunc<>nil then
   FileFunc(f.FlushFunc)(f);
end;


Procedure ReadLn_End(var f : TextRec);[Public,Alias: 'READLN_END'];
Begin
  If InOutRes <> 0 then exit;
  if not OpenInput(f) then
   exit;
{ Read until a linebreak }
  while (f.BufPos<f.BufEnd) do
   begin
     inc(f.BufPos);
     if (f.BufPtr^[f.BufPos-1]=#10) then
      exit;
     If f.BufPos>=f.BufEnd Then
      FileFunc(f.InOutFunc)(f);
   end;
{ Flush if set }
  if f.FlushFunc<>nil then
   FileFunc(f.FlushFunc)(f);
End;


{$ifdef VER0_99_5}
Procedure Read_String(var f : TextRec;var s : String);[Public,Alias: 'READ_TEXT_STRING'];
var
  Temp,sPos : Word;
Begin
  { Delete the string }
  s:='';
  If InOutRes <> 0 then exit;
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
      { Remove #13 from a #13#10 break }
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

{$else VER0_99_5}

Procedure Read_String(Maxlen : Longint;var f : TextRec;var s : String);[Public,Alias:'READ_TEXT_STRING'];
var
  Temp,sPos,nrread : Word;
Begin
  { Delete the string }
  s:='';
  If InOutRes <> 0 then exit;
  if not OpenInput(f) then
   exit;
  Temp:=f.BufPos;
  sPos:=1;
  NrRead:=0;
  while (f.BufPos<f.BufEnd) and ((f.Bufptr^[Temp]<>#10) and (NrRead<Maxlen)) Do
   Begin
   { search linefeed or length of string }
     while ((f.Bufptr^[Temp]<>#10) and (NrRead<Maxlen)) and (Temp<f.BufEnd) Do
      begin
      Temp:=Temp+1;
      NrRead:=NrRead+1;
      end;
   { copy String. Take 255 char limit in account.}
     If sPos+Temp-f.BufPos<=255 Then
      Begin
        Move (f.Bufptr^[f.BufPos],s[sPos],Temp-f.BufPos);
        sPos:=sPos+Temp-f.BufPos;
      { Remove #13 from a #13#10 break }
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
{$endif VER0_99_5}


Procedure Read_Char(var f : TextRec;var c : Char);[Public,Alias: 'READ_TEXT_CHAR'];
Begin
  c:=#0;
  If InOutRes <> 0 then exit;
  if not OpenInput(f) then
   exit;
  If f.BufPos>=f.BufEnd Then
   c:=#26
  else
   c:=f.Bufptr^[f.BufPos];
  Inc(f.BufPos);
End;


Procedure Read_PChar(var f : TextRec;var s : PChar);[Public,Alias:'READ_TEXT_PCHAR_AS_POINTER'];
var
  p    : PChar;
  Temp : byte;
Begin
{ Delete the string }
  s^:=#0;
  If InOutRes <> 0 then exit;
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


Procedure Read_Array(var f : TextRec;var s : array00);[Public,Alias:'READ_TEXT_PCHAR_AS_ARRAY'];
var
  p    : PChar;
  Temp : byte;
Begin
{ Delete the string }
  s[0]:=#0;
  If InOutRes <> 0 then exit;
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


{$ifdef useansistrings}
Procedure Read_String(Maxlen : Longint;var f : TextRec;var s : AnsiString);[Public,Alias: 'READ_TEXT_ANSISTRING'];
var
  p    : PChar;
  Temp : byte;
  len  : Longint;
Begin
{ Delete the string }
  Decr_ansi_ref (S);
  // We assign room for 1024 characters totally at random....
  Pointer(s):=Pointer(NewAnsiString(1024));
  If InOutRes <> 0 then exit;
  p:=pointer(s);
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
     Inc(len,Temp-f.bufpos);
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
  PAnsiRec(Pointer(S)-FirstOff)^.Len:=len
End;
{$endif}


Procedure Read_Longint(var f : TextRec;var l : Longint);[Public,Alias: 'READ_TEXT_LONGINT'];
var
  hs   : String;
  code : Word;
  base : longint;
Begin
  l:=0;
  If InOutRes <> 0 then exit;
  hs:='';
  if not OpenInput(f) then
   exit;
  if IgnoreSpaces(f) and ReadSign(f,hs) and ReadBase(f,hs,Base) then
   ReadNumeric(f,hs,Base);
  Val(hs,l,code);
  If code<>0 Then
   HandleError(106);
End;


Procedure Read_Integer(var f : TextRec;var l : Integer);[Public,Alias: 'READ_TEXT_INTEGER'];
var
  ll : Longint;
Begin
  l:=0;
  If InOutRes <> 0 then exit;
  Read_Longint(f,ll);
  If (ll<-32768) or (ll>32767) Then
   HandleError(106);
  l:=ll;
End;


Procedure Read_Word(var f : TextRec;var l : Word);[Public,Alias: 'READ_TEXT_WORD'];
var
  ll : Longint;
Begin
  l:=0;
  If InOutRes <> 0 then exit;
  Read_Longint(f,ll);
  If (ll<0) or (ll>$ffff) Then
   HandleError(106);
  l:=ll;
End;


Procedure Read_Byte(var f : TextRec;var l : byte);[Public,Alias: 'READ_TEXT_BYTE'];
var
  ll : Longint;
Begin
  l:=0;
  If InOutRes <> 0 then exit;
  Read_Longint(f,ll);
  If (ll<0) or (ll>255) Then
   HandleError(106);
  l:=ll;
End;


Procedure Read_Shortint(var f : TextRec;var l : shortint);[Public,Alias: 'READ_TEXT_SHORTINT'];
var
   ll : Longint;
Begin
  l:=0;
  If InOutRes <> 0 then exit;
  Read_Longint(f,ll);
  If (ll<-128) or (ll>127) Then
   HandleError(106);
  l:=ll;
End;


Procedure Read_Cardinal(var f : TextRec;var l : cardinal);[Public,Alias: 'READ_TEXT_CARDINAL'];
var
  hs   : String;
  code : Word;
  base : longint;
Begin
  l:=0;
  If InOutRes <> 0 then exit;
  hs:='';
  if not OpenInput(f) then
   exit;
  if IgnoreSpaces(f) and ReadSign(f,hs) and ReadBase(f,hs,Base) then
   ReadNumeric(f,hs,Base);
  val(hs,l,code);
  If code<>0 Then
   HandleError(106);
End;


Procedure Read_Real(var f : TextRec;var d : Real);[Public,Alias: 'READ_TEXT_REAL'];
var
  hs   : String;
  code : Word;
Begin
  d:=0.0;
  If InOutRes <> 0 then exit;
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
   HandleError(106);
End;


{$ifdef SUPPORT_EXTENDED}
Procedure Read_Extended(var f : TextRec;var d : extended);[Public,Alias: 'READ_TEXT_EXTENDED'];
var
  hs   : String;
  code : Word;
Begin
  d:=0.0;
  If InOutRes <> 0 then exit;
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
   HandleError(106);
End;
{$endif SUPPORT_EXTENDED}


{$ifdef SUPPORT_COMP}
Procedure Read_Comp(var f : TextRec;var d : comp);[Public,Alias: 'READ_TEXT_COMP'];
var
  hs   : String;
  code : Word;
Begin
  d:=comp(0.0);
  If InOutRes <> 0 then exit;
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
   HandleError(106);
End;
{$endif SUPPORT_COMP}


{$ifdef VER0_99_5}
Procedure r(var f : TextRec);[Public,Alias: 'READLN_TEXT'];
Begin
  If InOutRes <> 0 then exit;
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
{$endif VER0_99_5}


{*****************************************************************************
                               Initializing
*****************************************************************************}

procedure OpenStdIO(var f:text;mode:word;hdl:longint);
begin
  Assign(f,'');
  TextRec(f).Handle:=hdl;
  TextRec(f).Mode:=mode;
  TextRec(f).Closefunc:=@FileCloseFunc;
  case mode of
  fmInput : TextRec(f).InOutFunc:=@FileReadFunc;
 fmOutput : begin
              TextRec(f).InOutFunc:=@FileWriteFunc;
              TextRec(f).FlushFunc:=@FileWriteFunc;
            end;
  else
   HandleError(102);
  end;
end;


{
  $Log$
  Revision 1.24  1998-09-08 10:14:06  peter
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
