{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WithOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                    subroutines For UnTyped File handling
****************************************************************************}

type
  UnTypedFile=File;

procedure InitFile(var f : file);

begin
  FillChar(f,SizeOf(FileRec),0);
  FileRec(f).Handle:=UnusedHandle;
  FileRec(f).mode:=fmClosed;
end;

{$IFDEF FPC_UNICODE_RTL}
Procedure Assign(out f:File;const Name: UnicodeString);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
  InitFile(F);
  FileRec(f).Name:=Name;
End;
{$endif}
Procedure Assign(out f:File;const Name: RawByteString);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
  InitFile(F);
  FileRec(f).Name:=Name;
End;

Procedure Rewrite(var f:File;l:Longint);[IOCheck];
{
  Create file f with recordsize of l
}
Begin
  If InOutRes <> 0 then
   exit;
  Case FileRec(f).mode Of
   fmInOut,fmInput,fmOutput : Close(f);
   fmClosed : ;
  else
   Begin
     InOutRes:=102;
     exit;
   End;
  End;
  If l=0 Then
   InOutRes:=2
  else
   Begin
     { Reopen with filemode 2, to be Tp compatible (PFV) }
{$IFDEF FPC_UNICODE_RTL}
     Do_Open(f,UnicodeString(FileRec(f).Name),$1002);
{$ELSE}
     Do_Open(f,(FileRec(f).Name),$1002);
{$ENDIF}
     FileRec(f).RecSize:=l;
   End;
End;


Procedure Reset(var f:File;l:Longint);[IOCheck];
{
  Open file f with recordsize of l and filemode
}
Begin
  If InOutRes <> 0 then
   Exit;
  Case FileRec(f).mode Of
   fmInOut,fmInput,fmOutput : Close(f);
   fmClosed : ;
  else
   Begin
     InOutRes:=102;
     exit;
   End;
  End;
  If l=0 Then
   InOutRes:=2
  else
   Begin
{$IFDEF FPC_UNICODE_RTL}
     Do_Open(f,UnicodeString(FileRec(f).Name),Filemode);
{$ELSE}
     Do_Open(f,(FileRec(f).Name),FileMode);
{$ENDIF}
     FileRec(f).RecSize:=l;
   End;
End;


Procedure Rewrite(Var f:File);[IOCheck];
{
  Create file with (default) 128 byte records
}
Begin
  If InOutRes <> 0 then
   exit;
  Rewrite(f,128);
End;


Procedure Reset(Var f:File);[IOCheck];
{
  Open file with (default) 128 byte records
}
Begin
  If InOutRes <> 0 then
   exit;
  Reset(f,128);
End;


Procedure BlockWrite(Var f:File;Const Buf;Count:Int64;var Result:Int64);[IOCheck];
{
  Write Count records from Buf to file f, return written records in result
}
Begin
  Result:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmOutput :
      Result:=Do_Write(FileRec(f).Handle,@Buf,Count*FileRec(f).RecSize)
        div FileRec(f).RecSize;
    fmInPut: inOutRes := 105;
    else InOutRes:=103;
  end;
End;


Procedure BlockWrite(Var f:File;Const Buf;Count:Longint;var Result:Longint);[IOCheck];
{
  Write Count records from Buf to file f, return written records in result
}
var
  l : Int64;
Begin
  BlockWrite(f,Buf,Count,l);
  Result:=longint(l);
End;


Procedure BlockWrite(Var f:File;Const Buf;Count:Word;var Result:Word);[IOCheck];
{
  Write Count records from Buf to file f, return written records in Result
}
var
  l : Int64;
Begin
  BlockWrite(f,Buf,Count,l);
  Result:=word(l);
End;

Procedure BlockWrite(Var f:File;Const Buf;Count:Cardinal;var Result:Cardinal);[IOCheck];
{
  Write Count records from Buf to file f, return written records in Result
}
var
  l : Int64;
Begin
  BlockWrite(f,Buf,Count,l);
  Result:=l;
End;

Procedure BlockWrite(Var f:File;Const Buf;Count:Word;var Result:Integer);[IOCheck];
{
  Write Count records from Buf to file f, return written records in Result
}
var
  l : Int64;
Begin
  BlockWrite(f,Buf,Count,l);
  Result:=integer(l);
End;

Procedure BlockWrite(Var f:File;Const Buf;Count:Longint);[IOCheck];
{
  Write Count records from Buf to file f, if none a Read and Count>0 then
  InOutRes is set
}
var
  Result : Int64;
Begin
  BlockWrite(f,Buf,Count,Result);
  If (InOutRes=0) and (Result<Count) and (Count>0) Then
   InOutRes:=101;
End;

Procedure BlockRead(var f:File;var Buf;Count:Int64;var Result:Int64);[IOCheck];
{
  Read Count records from file f ro Buf, return number of read records in
  Result
}
Begin
  Result:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput :
      Result:=Do_Read(FileRec(f).Handle,@Buf,count*FileRec(f).RecSize)
        div FileRec(f).RecSize;
    fmOutput: inOutRes := 104;
    else InOutRes:=103;
  end;
End;

Procedure BlockRead(var f:File;var Buf;Count:Longint;var Result:Longint);[IOCheck];
{
  Read Count records from file f ro Buf, return number of read records in
  Result
}
var
  l : int64;
Begin
  BlockRead(f,Buf,Count,l);
  Result:=longint(l);
End;

Procedure BlockRead(var f:File;var Buf;count:Word;var Result:Word);[IOCheck];
{
  Read Count records from file f to Buf, return number of read records in
  Result
}
var
  l : int64;
Begin
  BlockRead(f,Buf,Count,l);
  Result:=word(l);
End;

Procedure BlockRead(var f:File;var Buf;count:Cardinal;var Result:Cardinal);[IOCheck];
{
  Read Count records from file f to Buf, return number of read records in
  Result
}
var
  l : int64;
Begin
  BlockRead(f,Buf,Count,l);
  Result:=l;
End;

Procedure BlockRead(var f:File;var Buf;count:Word;var Result:Integer);[IOCheck];
{
  Read Count records from file f to Buf, return number of read records in
  Result
}
var
  l : int64;
Begin
  BlockRead(f,Buf,Count,l);
  Result:=integer(l);
End;

Procedure BlockRead(Var f:File;Var Buf;Count:Int64);[IOCheck];
{
  Read Count records from file f to Buf, if none are read and Count>0 then
  InOutRes is set
}
var
  Result : int64;
Begin
  BlockRead(f,Buf,Count,Result);
  If (InOutRes=0) and (Result<Count) and (Count>0) Then
   InOutRes:=100;
End;


Function FilePos(var f:File):Int64;[IOCheck];
{
  Return current Position In file f in records
}
Begin
  FilePos:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput :
      FilePos:=Do_FilePos(FileRec(f).Handle) div FileRec(f).RecSize;
    else
      InOutRes:=103;
  end;
End;


Function FileSize(var f:File):Int64;[IOCheck];
{
  Return the size of file f in records
}
Begin
  FileSize:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput :
      begin
        if (FileRec(f).RecSize>0) then
          FileSize:=Do_FileSize(FileRec(f).Handle) div FileRec(f).RecSize;
      end;
    else InOutRes:=103;
  end;
End;


Function Eof(var f:File):Boolean;[IOCheck];
{
  Return True if we're at the end of the file f, else False is returned
}
Begin
  Eof:=false;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    {Can't use do_ routines because we need record support}
    fmInOut,fmInput,fmOutput : Eof:=(FileSize(f)<=FilePos(f));
    else InOutRes:=103;
  end;
End;


Procedure Seek(var f:File;Pos:Int64);[IOCheck];
{
  Goto record Pos in file f
}
Begin
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput :
      Do_Seek(FileRec(f).Handle,Pos*FileRec(f).RecSize);
    else InOutRes:=103;
  end;
End;

Procedure Truncate(Var f:File);[IOCheck];
{
  Truncate/Cut file f at the current record Position
}
Begin
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmOutput :
      Do_Truncate(FileRec(f).Handle,FilePos(f)*FileRec(f).RecSize);
    else InOutRes:=103;
  end;
End;


Procedure Close(var f:File);[IOCheck];
{
  Close file f
}
Begin
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput :
      begin
        Do_Close(FileRec(f).Handle);
        FileRec(f).mode:=fmClosed;
      end
    else InOutRes:=103;
  end;
End;


Procedure Erase(var f : File);[IOCheck];
Begin
  If InOutRes <> 0 then
   exit;
  If FileRec(f).mode=fmClosed Then
{$IFDEF FPC_UNICODE_RTL}
   Do_Erase(UnicodeString(FileRec(f).Name));
{$ELSE}
   Do_Erase(FileRec(f).Name);
{$ENDIF}
End;


{$IFDEF FPC_UNICODE_RTL}
Procedure Rename(var f : File; const S : UnicodeString);[IOCheck];

Begin
  If InOutRes <> 0 then
   exit;
  Do_Rename(FileRec(f).Name,S);
  If InOutRes = 0 then
     FileRec(f).Name:=S
End;
{$endif}

Procedure Rename(var f : File;const s : rawbytestring);[IOCheck];

Begin
  If InOutRes <> 0 then
   exit;
  Do_Rename(FileRec(f).Name,S);
  If InOutRes = 0 then
     FileRec(f).Name:=S
End;

