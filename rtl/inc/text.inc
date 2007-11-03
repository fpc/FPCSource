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
  { prevent unecessary system call }
  if t.BufPos=0 then
    exit;
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


Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} t:Text;const s:String);
Begin
  FillChar(t,SizeOf(TextRec),0);
{ only set things that are not zero }
  TextRec(t).Handle:=UnusedHandle;
  TextRec(t).mode:=fmClosed;
  TextRec(t).BufSize:=TextRecBufSize;
  TextRec(t).Bufptr:=@TextRec(t).Buffer;
  TextRec(t).OpenFunc:=@FileOpenFunc;
  Case DefaultTextLineBreakStyle Of
    tlbsLF: TextRec(t).LineEnd := #10;
    tlbsCRLF: TextRec(t).LineEnd := #13#10;
    tlbsCR: TextRec(t).LineEnd := #13;
  End;
  Move(s[1],TextRec(t).Name,Length(s));
End;


Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} t:Text;p:pchar);
begin
  Assign(t,StrPas(p));
end;


Procedure Assign({$ifdef PARAOUTFILE}out{$else}var{$endif} t:Text;c:char);
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
  oldfilepos : Int64;
  oldbufpos, oldbufend : SizeInt;
  reads: longint;
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
      #26 :
        if CtrlZMarksEOF then
          begin
            SeekEof := true;
            break;
          end;
     #10,#13,#9,' ' :
       ;
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


Procedure SetTextBuf(Var F : Text; Var Buf; Size : SizeInt);
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


Function fpc_get_input:PText;compilerproc;
begin
  fpc_get_input:=@Input;
end;


Function fpc_get_output:PText;compilerproc;
begin
  fpc_get_output:=@Output;
end;


{*****************************************************************************
                               Write(Ln)
*****************************************************************************}

Procedure fpc_WriteBuffer(var f:Text;const b;len:SizeInt);[Public,Alias:'FPC_WRITEBUFFER'];
var
  p   : pchar;
  left,
  idx : SizeInt;
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


Procedure fpc_Write_End(var f:Text);[Public,Alias:'FPC_WRITE_END']; iocheck; compilerproc;
begin
  if TextRec(f).FlushFunc<>nil then
   FileFunc(TextRec(f).FlushFunc)(TextRec(f));
end;


Procedure fpc_Writeln_End(var f:Text);[Public,Alias:'FPC_WRITELN_END']; iocheck; compilerproc;
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


Procedure fpc_Write_Text_ShortStr(Len : Longint;var f : Text;const s : String); iocheck; [Public,Alias:'FPC_WRITE_TEXT_SHORTSTR']; compilerproc;
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

Procedure fpc_Write_Text_Pchar_as_Array(Len : Longint;var f : Text;const s : array of char; zerobased: boolean = true); iocheck; [Public,Alias:'FPC_WRITE_TEXT_PCHAR_AS_ARRAY']; compilerproc;
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
        if (zerobased) then
          begin
            { can't use StrLen, since that one could try to read past the end }
            { of the heap (JM)                                                }
            ArrayLen:=IndexByte(p^,high(s)+1,0);
            { IndexByte returns -1 if not found (JM) }
            if ArrayLen = -1 then
              ArrayLen := high(s)+1;
          end
        else
          ArrayLen := high(s)+1;
        If Len>ArrayLen Then
          fpc_WriteBlanks(f,Len-ArrayLen);
        fpc_WriteBuffer(f,p^,ArrayLen);
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
End;


Procedure fpc_Write_Text_PChar_As_Pointer(Len : Longint;var f : Text;p : PChar); iocheck; [Public,Alias:'FPC_WRITE_TEXT_PCHAR_AS_POINTER']; compilerproc;
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


Procedure fpc_Write_Text_AnsiStr (Len : Longint; Var f : Text; const S : AnsiString); iocheck; [Public,alias:'FPC_WRITE_TEXT_ANSISTR']; compilerproc;
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


Procedure fpc_Write_Text_WideStr (Len : Longint; Var f : Text; const S : WideString); iocheck; [Public,alias:'FPC_WRITE_TEXT_WIDESTR']; compilerproc;
{
 Writes a WideString to the Text file T
}
var
  SLen : longint;
  a: ansistring;
begin
  If (pointer(S)=nil) or (InOutRes<>0) then
   exit;
  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
      begin
        SLen:=Length(s);
        If Len>SLen Then
          fpc_WriteBlanks(f,Len-SLen);
        a:=s;
        { length(a) can be > slen, e.g. after utf-16 -> utf-8 }
        fpc_WriteBuffer(f,pchar(a)^,length(a));
      end;
    fmInput: InOutRes:=105
    else InOutRes:=103;
  end;
end;


Procedure fpc_Write_Text_SInt(Len : Longint;var t : Text;l : ValSInt); iocheck; [Public,Alias:'FPC_WRITE_TEXT_SINT']; compilerproc;
var
  s : String;
Begin
  If (InOutRes<>0) then
   exit;
  Str(l,s);
  Write_Str(Len,t,s);
End;


Procedure fpc_Write_Text_UInt(Len : Longint;var t : Text;l : ValUInt); iocheck; [Public,Alias:'FPC_WRITE_TEXT_UINT']; compilerproc;
var
  s : String;
Begin
  If (InOutRes<>0) then
   exit;
  Str(L,s);
  Write_Str(Len,t,s);
End;


{$ifndef CPU64}

procedure fpc_write_text_qword(len : longint;var t : text;q : qword); iocheck; [public,alias:'FPC_WRITE_TEXT_QWORD']; compilerproc;
var
  s : string;
begin
  if (InOutRes<>0) then
   exit;
  str(q,s);
  write_str(len,t,s);
end;

procedure fpc_write_text_int64(len : longint;var t : text;i : int64); iocheck; [public,alias:'FPC_WRITE_TEXT_INT64']; compilerproc;
var
  s : string;
begin
  if (InOutRes<>0) then
   exit;
  str(i,s);
  write_str(len,t,s);
end;

{$endif CPU64}

Procedure fpc_Write_Text_Float(rt,fixkomma,Len : Longint;var t : Text;r : ValReal); iocheck; [Public,Alias:'FPC_WRITE_TEXT_FLOAT']; compilerproc;
var
  s : String;
Begin
  If (InOutRes<>0) then
   exit;
  Str_real(Len,fixkomma,r,treal_type(rt),s);
  Write_Str(Len,t,s);
End;

procedure fpc_write_text_enum(typinfo,ord2strindex:pointer;len:sizeint;var t:text;ordinal:longint); iocheck; [Public,Alias:'FPC_WRITE_TEXT_ENUM']; compilerproc;

type  Ptypeinfo=^Ttypeinfo;
      Ttypeinfo=record
        kind:byte;
        name:shortstring;
       end;

      Penuminfo=^Tenuminfo;
      Tenuminfo={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
        ordtype:byte;
        minvalue,maxvalue:longint;
        basetype:pointer;
        namelist:shortstring;
      end;

      Tsorted_array={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
        o:longint;
        s:Pstring;
      end;

var
    p:Pstring;
    l,h,m:cardinal;
    sorted_array:^Tsorted_array;
    s:string;

begin
  if textrec(t).mode<>fmoutput then
    begin
      if textrec(t).mode=fminput then
        inoutres:=105
      else
        inoutres:=103;
      exit;
    end;
  if Pcardinal(ord2strindex)^=0 then
    begin
      {The compiler did generate a lookup table.}
      with Penuminfo(Pbyte(typinfo)+2+length(Ptypeinfo(typinfo)^.name))^ do
        begin
          if (ordinal<minvalue) or (ordinal>maxvalue) then
            begin
              inoutres:=107;  {Invalid ordinal value for this enum.}
              exit;
            end;
          dec(ordinal,minvalue);
        end;
      {Get the address of the string.}
      p:=Pshortstring((PPpointer(ord2strindex)+1+ordinal)^);
      if p=nil then
        begin
          inoutres:=107;      {Invalid ordinal value for this enum.}
          exit;
        end;
      s:=p^;
    end
  else
    begin
      {The compiler did generate a sorted array of (ordvalue,Pstring) tuples.}
      sorted_array:=pointer(Pcardinal(ord2strindex)+2);
      {Use a binary search to get the string.}
      l:=0;
      h:=(Pcardinal(ord2strindex)+1)^-1;
      repeat
        m:=(l+h) div 2;
        if ordinal>sorted_array[m].o then
          l:=m+1
        else if ordinal<sorted_array[m].o then
          h:=m-1
        else
          break;
        if l>h then
          begin
            inoutres:=107;      {Invalid ordinal value for this enum.}
            exit;
          end;
      until false;
      s:=sorted_array[m].s^;
    end;
  fpc_writeBuffer(t,s[1],length(s));
  {Pad the string with spaces if necessary.}
  if len>length(s) then
    fpc_writeblanks(t,len-length(s));
end;

{$ifdef FPC_HAS_STR_CURRENCY}
Procedure fpc_Write_Text_Currency(fixkomma,Len : Longint;var t : Text;c : Currency); iocheck; [Public,Alias:'FPC_WRITE_TEXT_CURRENCY']; compilerproc;
var
  s : String;
Begin
  If (InOutRes<>0) then
   exit;
  str(c:Len:fixkomma,s);
  Write_Str(Len,t,s);
End;
{$endif FPC_HAS_STR_CURRENCY}

Procedure fpc_Write_Text_Boolean(Len : Longint;var t : Text;b : Boolean); iocheck; [Public,Alias:'FPC_WRITE_TEXT_BOOLEAN']; compilerproc;
Begin
  If (InOutRes<>0) then
   exit;
{ Can't use array[boolean] because b can be >0 ! }
  if b then
    Write_Str(Len,t,'TRUE')
  else
    Write_Str(Len,t,'FALSE');
End;


Procedure fpc_Write_Text_Char(Len : Longint;var t : Text;c : Char); iocheck; [Public,Alias:'FPC_WRITE_TEXT_CHAR']; compilerproc;
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
  If TextRec(t).BufPos>=TextRec(t).BufSize Then
    FileFunc(TextRec(t).InOutFunc)(TextRec(t));
  TextRec(t).Bufptr^[TextRec(t).BufPos]:=c;
  Inc(TextRec(t).BufPos);
End;


Procedure fpc_Write_Text_WideChar(Len : Longint;var t : Text;c : WideChar); iocheck; [Public,Alias:'FPC_WRITE_TEXT_WIDECHAR']; compilerproc;
var
  a : ansistring;
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
  If TextRec(t).BufPos>=TextRec(t).BufSize Then
    FileFunc(TextRec(t).InOutFunc)(TextRec(t));
  { a widechar can be translated into more than a single ansichar }
  a:=c;
  fpc_WriteBuffer(t,pchar(a)^,length(a));
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


function CheckRead(var f:Text):Boolean;
begin
  CheckRead:=False;
{ Check error and if file is open and load buf if empty }
  If (InOutRes<>0) then
    exit;
  if (TextRec(f).mode<>fmInput) Then
    begin
      case TextRec(f).mode of
        fmOutPut,fmAppend:
          InOutRes:=104;
        else
          InOutRes:=103;
      end;
      exit;
    end;
  if TextRec(f).BufPos>=TextRec(f).BufEnd Then
    FileFunc(TextRec(f).InOutFunc)(TextRec(f));
  CheckRead:=True;
end;


Procedure fpc_Read_End(var f:Text);[Public,Alias:'FPC_READ_END']; iocheck; compilerproc;
begin
  if TextRec(f).FlushFunc<>nil then
   FileFunc(TextRec(f).FlushFunc)(TextRec(f));
end;


Procedure fpc_ReadLn_End(var f : Text);[Public,Alias:'FPC_READLN_END']; iocheck; compilerproc;
var prev: char;
Begin
  If not CheckRead(f) then
    exit;
  if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
    { Flush if set }
    begin
      if (TextRec(f).FlushFunc<>nil) then
        FileFunc(TextRec(f).FlushFunc)(TextRec(f));
      exit;
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
  end_of_string:boolean;
Begin
  ReadPCharLen:=0;
  If not CheckRead(f) then
    exit;
{ Read maximal until Maxlen is reached }
  sPos:=0;
  end_of_string:=false;
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
  { find stop character }
    while p<maxp do
      begin
        { Optimization: Do a quick check for a control character first }
        if (p^<' ') then
          begin
            if (p^ in [#10,#13]) or
               (ctrlZmarkseof and (p^=#26)) then
              begin
                end_of_string:=true;
                break;
              end;
          end;
        inc(p);
      end;
  { calculate read bytes }
    len:=p-startp;
    inc(TextRec(f).BufPos,Len);
    Move(startp^,s[sPos],Len);
    inc(sPos,Len);
  until (spos=MaxLen) or end_of_string;
  ReadPCharLen:=spos;
End;


Procedure fpc_Read_Text_ShortStr(var f : Text;out s : String); iocheck; [Public,Alias:'FPC_READ_TEXT_SHORTSTR']; compilerproc;
Begin
  s[0]:=chr(ReadPCharLen(f,pchar(@s[1]),high(s)));
End;


Procedure fpc_Read_Text_PChar_As_Pointer(var f : Text; const s : PChar); iocheck; [Public,Alias:'FPC_READ_TEXT_PCHAR_AS_POINTER']; compilerproc;
Begin
  pchar(s+ReadPCharLen(f,s,$7fffffff))^:=#0;
End;


Procedure fpc_Read_Text_PChar_As_Array(var f : Text;out s : array of char; zerobased: boolean = false); iocheck; [Public,Alias:'FPC_READ_TEXT_PCHAR_AS_ARRAY']; compilerproc;
var
  len: longint;
Begin
  len := ReadPCharLen(f,pchar(@s),high(s)+1);
  if zerobased and
     (len > high(s)) then
    len := high(s);
  if (len <= high(s)) then
    s[len] := #0;
End;


Procedure fpc_Read_Text_AnsiStr(var f : Text;out s : AnsiString); iocheck; [Public,Alias:'FPC_READ_TEXT_ANSISTR']; compilerproc;
var
  slen,len : SizeInt;
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

procedure fpc_Read_Text_Char(var f : Text; out c: char); iocheck; [Public,Alias:'FPC_READ_TEXT_CHAR'];compilerproc;
Begin
  c:=#0;
  If not CheckRead(f) then
    exit;
  If TextRec(f).BufPos>=TextRec(f).BufEnd Then
    begin
      c := #26;
      exit;
    end;
  c:=TextRec(f).Bufptr^[TextRec(f).BufPos];
  inc(TextRec(f).BufPos);
end;


Procedure fpc_Read_Text_SInt(var f : Text; out l : ValSInt); iocheck; [Public,Alias:'FPC_READ_TEXT_SINT']; compilerproc;
var
  hs   : String;
  code : longint;
Begin
  l:=0;
  If not CheckRead(f) then
    exit;
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
   if (hs = '') then
    L := 0
   else
    begin
     Val(hs,l,code);
     if Code <> 0 then
      InOutRes:=106;
    end;
End;


Procedure fpc_Read_Text_UInt(var f : Text; out u : ValUInt);  iocheck; [Public,Alias:'FPC_READ_TEXT_UINT']; compilerproc;
var
  hs   : String;
  code : longint;
Begin
  u:=0;
  If not CheckRead(f) then
    exit;
  hs:='';
  if IgnoreSpaces(f) then
   begin
     { When spaces were found and we are now at EOF,
       then we return 0 }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      exit;
     ReadNumeric(f,hs);
   end;
   if (hs = '') then
    u := 0
   else
    begin
      val(hs,u,code);
      If code<>0 Then
        InOutRes:=106;
    end;
End;


procedure fpc_Read_Text_Float(var f : Text; out v : ValReal); iocheck; [Public,Alias:'FPC_READ_TEXT_FLOAT']; compilerproc;
var
  hs : string;
  code : Word;
begin
  v:=0.0;
  If not CheckRead(f) then
    exit;
  hs:='';
  if IgnoreSpaces(f) then
   begin
     { When spaces were found and we are now at EOF,
       then we return 0 }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      exit;
     ReadNumeric(f,hs);
   end;
  val(hs,v,code);
  If code<>0 Then
   InOutRes:=106;
end;

procedure fpc_read_text_enum(str2ordindex:pointer;var t:text;out ordinal:longint); iocheck; [Public,Alias:'FPC_READ_TEXT_ENUM'];compilerproc;

var s:string;
    code:valsint;

begin
  if not checkread(t) then
    exit;
  s:='';
  if ignorespaces(t) then
    begin
      { When spaces were found and we are now at EOF, then we return 0 }
      if (TextRec(t).BufPos>=TextRec(t).BufEnd) then
        exit;
      ReadNumeric(t,s);
    end;
  ordinal:=fpc_val_enum_shortstr(str2ordindex,s,code);
  if code<>0 then
   InOutRes:=106;
end;

procedure fpc_Read_Text_Currency(var f : Text; out v : Currency); iocheck; [Public,Alias:'FPC_READ_TEXT_CURRENCY']; compilerproc;
var
  hs : string;
  code : Word;
begin
  v:=0.0;
  If not CheckRead(f) then
    exit;
  hs:='';
  if IgnoreSpaces(f) then
   begin
     { When spaces were found and we are now at EOF,
       then we return 0 }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      exit;
     ReadNumeric(f,hs);
   end;
  val(hs,v,code);
  If code<>0 Then
   InOutRes:=106;
end;


{$ifndef cpu64}

procedure fpc_Read_Text_QWord(var f : text; out q : qword); iocheck; [public,alias:'FPC_READ_TEXT_QWORD']; compilerproc;
var
  hs   : String;
  code : longint;
Begin
  q:=0;
  If not CheckRead(f) then
    exit;
  hs:='';
  if IgnoreSpaces(f) then
   begin
     { When spaces were found and we are now at EOF,
       then we return 0 }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      exit;
     ReadNumeric(f,hs);
   end;
  val(hs,q,code);
  If code<>0 Then
   InOutRes:=106;
End;

procedure fpc_Read_Text_Int64(var f : text; out i : int64); iocheck; [public,alias:'FPC_READ_TEXT_INT64']; compilerproc;
var
  hs   : String;
  code : Longint;
Begin
  i:=0;
  If not CheckRead(f) then
    exit;
  hs:='';
  if IgnoreSpaces(f) then
   begin
     { When spaces were found and we are now at EOF,
       then we return 0 }
     if (TextRec(f).BufPos>=TextRec(f).BufEnd) then
      exit;
     ReadNumeric(f,hs);
   end;
  Val(hs,i,code);
  If code<>0 Then
   InOutRes:=106;
End;

{$endif CPU64}



{*****************************************************************************
                              WriteStr/ReadStr
*****************************************************************************}

const
  StrPtrIndex = 1;
  { leave space for 128 bit string pointers :) (used for writestr) }
  ShortStrLenIndex = 17;
  { how many bytes of the string have been processed already (used for readstr) }
  BytesReadIndex = 17;

threadvar
  ReadWriteStrText: textrec;

procedure WriteStrShort(var t: textrec);
var
  str: pshortstring;
  newbytes,
  oldlen: longint;
begin
  if (t.bufpos=0) then
    exit;
  str:=pshortstring(ppointer(@t.userdata[StrPtrIndex])^);
  newbytes:=t.BufPos;
  oldlen:=length(str^);
  if (oldlen+t.bufpos > t.userdata[ShortStrLenIndex]) then
    begin
      newbytes:=t.userdata[ShortStrLenIndex]-oldlen;
{$ifdef writestr_iolencheck}
      // GPC only gives an io error if {$no-truncate-strings} is active
      // FPC does not have this setting (it never gives errors when a
      // a string expression is truncated)

      { "disk full" }
      inoutres:=101;
{$endif}
    end;
  setlength(str^,length(str^)+newbytes);
  move(t.bufptr^,str^[oldlen+1],newbytes);
  t.bufpos:=0;
end;


procedure WriteStrAnsi(var t: textrec);
var
  str: pansistring;
  oldlen: longint;
begin
  if (t.bufpos=0) then
    exit;
  str:=pansistring(ppointer(@t.userdata[StrPtrIndex])^);
  oldlen:=length(str^);
  setlength(str^,oldlen+t.bufpos);
  move(t.bufptr^,str^[oldlen+1],t.bufpos);
  t.bufpos:=0;
end;


procedure WriteStrWide(var t: textrec);
var
  temp: ansistring;
  str: pwidestring;
begin
  if (t.bufpos=0) then
    exit;
  str:=pwidestring(ppointer(@t.userdata[StrPtrIndex])^);
  setlength(temp,t.bufpos);
  move(t.bufptr^,temp[1],t.bufpos);
  str^:=str^+temp;
  t.bufpos:=0;
end;


procedure SetupWriteStrCommon(out t: textrec);
begin
  // initialise
  Assign(text(t),'');
  t.mode:=fmOutput;
  t.OpenFunc:=nil;
  t.CloseFunc:=nil;
end;


function fpc_SetupWriteStr_Shortstr(out s: shortstring): PText; compilerproc;
begin
  setupwritestrcommon(ReadWriteStrText);
  PPointer(@ReadWriteStrText.userdata[StrPtrIndex])^:=@s;
  ReadWriteStrText.userdata[ShortStrLenIndex]:=high(s);
  setlength(s,0);
  ReadWriteStrText.InOutFunc:=@WriteStrShort;
  ReadWriteStrText.FlushFunc:=@WriteStrShort;
  result:=@ReadWriteStrText;
end;


function fpc_SetupWriteStr_Ansistr(out s: ansistring): PText; compilerproc;
begin
  setupwritestrcommon(ReadWriteStrText);
  PPointer(@ReadWriteStrText.userdata[StrPtrIndex])^:=@s;
// automatically done by out-semantics
//  setlength(s,0);
  ReadWriteStrText.InOutFunc:=@WriteStrAnsi;
  ReadWriteStrText.FlushFunc:=@WriteStrAnsi;
  result:=@ReadWriteStrText;
end;


function fpc_SetupWriteStr_Widestr(out s: widestring): PText; compilerproc;
begin
  setupwritestrcommon(ReadWriteStrText);
  PPointer(@ReadWriteStrText.userdata[StrPtrIndex])^:=@s;
// automatically done by out-semantics
//  setlength(s,0);
  ReadWriteStrText.InOutFunc:=@WriteStrWide;
  ReadWriteStrText.FlushFunc:=@WriteStrWide;
  result:=@ReadWriteStrText;
end;



procedure ReadAnsiStrFinal(var t: textrec);
begin
  { finalise the temp ansistring }
  PAnsiString(@t.userdata[StrPtrIndex])^ := '';
end;


procedure ReadStrCommon(var t: textrec; strdata: pchar; len: sizeint);
var
  newbytes: sizeint;
begin
  newbytes := len - PSizeInt(@t.userdata[BytesReadIndex])^;
  if (t.BufSize <= newbytes) then
    newbytes := t.BufSize;
  if (newbytes > 0) then
    begin
      move(strdata[PSizeInt(@t.userdata[BytesReadIndex])^],t.BufPtr^,newbytes);
      inc(PSizeInt(@t.userdata[BytesReadIndex])^,newbytes);
    end;
  t.BufEnd:=newbytes;
  t.BufPos:=0;
end;


procedure ReadStrAnsi(var t: textrec);
var
  str: pansistring;
begin
  str:=pansistring(@t.userdata[StrPtrIndex]);
  ReadStrCommon(t,@str^[1],length(str^));
end;


procedure SetupReadStrCommon(out t: textrec);
begin
  // initialise
  Assign(text(t),'');
  t.mode:=fmInput;
  t.OpenFunc:=nil;
  t.CloseFunc:=nil;
  PSizeInt(@t.userdata[BytesReadIndex])^:=0;
end;


function fpc_SetupReadStr_Ansistr(const s: ansistring): PText; [public, alias: 'FPC_SETUPREADSTR_ANSISTR']; compilerproc;
begin
  setupreadstrcommon(ReadWriteStrText);
  { we need a reference, because 's' may be a temporary expression }
  PAnsiString(@ReadWriteStrText.userdata[StrPtrIndex])^:=s;
  ReadWriteStrText.InOutFunc:=@ReadStrAnsi;
  { this is called at the end, by fpc_read_end }
  ReadWriteStrText.FlushFunc:=@ReadAnsiStrFinal;
  result:=@ReadWriteStrText;
end;

function fpc_SetupReadStr_Ansistr_Intern(const s: ansistring): PText; [external name 'FPC_SETUPREADSTR_ANSISTR'];


function fpc_SetupReadStr_Shortstr(const s: shortstring): PText; compilerproc;
begin
  { the reason we convert the short string to ansistring, is because the semantics of
    readstr are defined as:

    *********************
    Apart from the restrictions imposed by requirements given in this clause,
    the execution of readstr(e,v 1 ,...,v n ) where e denotes a
    string-expression and v 1 ,...,v n denote variable-accesses possessing the
    char-type (or a subrange of char-type), the integer-type (or a subrange of
    integer-type), the real-type, a fixed-string-type, or a
    variable-string-type, shall be equivalent to

            begin
            rewrite(f);
            writeln(f, e);
            reset(f);
            read(f, v 1 ,...,v n )
            end
    *********************

    This means that any side effects caused by the evaluation of v 1 .. v n
    must not affect the value of e (= our argument s) -> we need a copy of it.
    An ansistring is the easiest way to get a threadsafe copy, and allows us
    to use the other ansistring readstr helpers too.
  }
  result:=fpc_SetupReadStr_Ansistr_Intern(s);
end;


function fpc_SetupReadStr_Widestr(const s: widestring): PText; compilerproc;
begin
  { we use an ansistring to avoid code duplication, and let the    }
  { assignment convert the widestring to an equivalent ansistring  }
  result:=fpc_SetupReadStr_Ansistr_Intern(s);
end;


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
        if Do_Isdevice(hdl) then
          TextRec(f).FlushFunc:=@FileWriteFunc;
      end;
  else
   HandleError(102);
  end;
end;


