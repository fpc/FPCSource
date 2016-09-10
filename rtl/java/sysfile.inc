{
    This file is part of the Free Pascal run time library.

    Main OS dependant body of the system unit, loosely modelled
    after POSIX.  *BSD version (Linux version is near identical)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Const
  UnusedHandle = Nil;
  DefaultTextLineBreakStyle = tlbsLF;
  CtrlZMarksEOF: boolean = false;


Procedure Do_Close(Handle:thandle);

Begin
  Try
    (JLObject(Handle) as JICloseable).close();
  except
    InoutRes:=1
  end;  
End;

Procedure Do_Erase(p: PFileTextRecChar; pchangeable: boolean);

var
  F : JIFile;
  S : String;
  
Begin
  InoutRes:=0;
  S:=P;
  F:=JIFile.Create(S);
  if Not F.Exists then
    InoutRes:=2
  else
    if F.isDirectory then
      InoutRes:=2
    else    
      try
        F.Delete;
      except
        InoutRes:=5;
      end;    
End;

{ truncate at a given position }
procedure do_truncate (handle:thandle;fpos:int64);

Var
  F : JIFile;
  C : JNCFileChannel;
  
begin
  InoutRes:=0;
  try
    // Will raise an error for stdout, since it is JIPrintStream, not JIFileOutputStream
    C:=(JLObject(handle) as JIFileOutputStream).getChannel();
    C.truncate(fpos);
  except
    InoutRes:=2;
  end;  
end;



Procedure Do_Rename(p1 : PFileTextRecChar ; p2:UnicodeString; p1changeable, p2changeable: boolean);

Var
  S1,S2 : string;
  F1,F2 : JIFile;

Begin
  S1:=P1;
  S2:=P2;
  F1:=JIFile.Create(S1);
  F2:=JIFile.Create(S2);
  if not F1.Exists then
    InoutRes:=2
  else
    try
      F1.RenameTo(F2);
      InoutRes:=1;
    except
      InoutRes:=5;
    end;  
End;


Function Do_Write(Handle:thandle;Addr:Pointer;Len:Longint):longint;

var 
  F : JIOutputStream;

Begin
  InoutRes:=0;
  F:=JIOutputStream(Handle);
  try
    F.write(Arr1jbyte(Addr),0,Len);
    Do_Write:=Len;
  except
    InoutRes:=101;  
  end;
End;


Function Do_Read(Handle:thandle;Addr:Pointer;Len:Longint):Longint;

var
  F : JIInputStream;

Begin
  InoutRes:=0;
  F:=JIInputStream(Handle);
  try
    Do_Read:=F.read(Arr1jbyte(Addr),0,Len);
  except
    InoutRes:=101;
  end;  
End;

function Do_FilePos(Handle: thandle):Int64;

Var
  C : JNCFileChannel;

Begin
  Do_FilePos:=0;
  if JLObject(Handle) is JIFileInputStream then
    C:=JIFileInputStream(Handle).getChannel()
  else if JLObject(Handle) is JIFileOutputStream then
    C:=JIFileOutputStream(Handle).getChannel()
  else
    C:=Nil;
  try
    if (C<>Nil) then
      begin
      Do_FilePos:=C.position();
      InoutRes:=0;
      end
    else
      InoutRes:=5;
  except
    InoutRes:=5;
  end;
End;

Procedure Do_Seek(Handle:thandle;Pos:Int64);

Var
  C : JNCFileChannel;

Begin
  if JLObject(Handle) is JIFileInputStream then
    C:=JIFileInputStream(Handle).getChannel()
  else
    C:=JIFileOutputStream(Handle).getChannel();
  try
    C.position(Pos);
    InoutRes:=0;
  except
    InoutRes:=5;
  end;
End;

Function GetChannel(Handle:thandle):JNCFileChannel;

begin
  if JLObject(Handle) is JIFileInputStream then
    GetChannel:=JIFileInputStream(Handle).getChannel()
  else if JLObject(Handle) is JIFileOutputStream then
    GetChannel:=JIFileOutputStream(Handle).getChannel()
  else if JLObject(Handle) is JIRandomAccessFile then
    GetChannel:=JIRandomAccessFile(Handle).getChannel()
  else
    begin
    GetChannel:=Nil;
    InoutRes:=5;
    end;
end;

Function Do_SeekEnd(Handle:thandle):Int64;

Var
  C : JNCFileChannel;

Begin
  C:=GetChannel(Handle); // Sets inoutres if Nil
  try
    if Assigned(C) then
      begin
      C.position(C.Size);
      InoutRes:=0;
      end
  except
    InoutRes:=5;
  end;
end;

Function Do_FileSize(Handle:thandle):Int64;

Var
  C : JNCFileChannel;

Begin
  C:=GetChannel(Handle);  // Sets inoutres if Nil
  try
    if Assigned(C) then
      begin
      Result:=C.Size;
      InoutRes:=0;
      end
  except
    InoutRes:=5;
  end;
End;

Function Do_IsDevice(Handle : THandle) : boolean;

begin
  Result:=JLObject(Handle) is JIPrintStream ;
end;


Procedure Do_Open(var F : TextRec; p: PFileTextRecChar; flags: longint; pchangeable: boolean);
{
  FileRec and textrec have both Handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  S : String;
Begin
  S:=p;
{ close first if opened }
  if ((flags and $10000)=0) then
   begin
     case f.mode of
      fminput,fmoutput,fminout : Do_Close(TextRec(f).Handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file Handle }
  f.Handle:=Nil;
{ We do the conversion of filemodes here, concentrated on 1 place }
  case (flags and 3) of
   0 : begin
       if S='' then
         f.Handle:=JLsystem.fin
       else
         F.Handle:=JIFileInputStream.create(S);
       f.mode:=fminput;
       end;
   1 : begin
       if S='' then
         f.Handle:=JLsystem.fout
       else
         F.Handle:=JIFileOutputStream.Create(S,(flags and $100)=$100);
       f.mode:=fmoutput;
       end;
   2 : begin
       // We may need to check existence first ?
       F.Handle:=JIRandomAccessFile.Create(S,'rw');
       if (flags and $100)=$100 then
         With GetChannel(F.Handle) do
           Position(Size);
       f.mode:=fminout;
       end;
  end;
  if (S='') then
    exit;
  If f.Handle=Nil Then
    begin
    InoutRes:=5;
    f.mode:=fmclosed;
   end
  else
   InOutRes:=0;
End;


