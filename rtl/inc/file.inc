{
    $Id$
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

Procedure Assign(var f:File;const Name:string);
{
  Assign Name to file f so it can be used with the file routines
}
Begin
  FillChar(f,SizeOf(FileRec),0);
  FileRec(f).Handle:=UnusedHandle;
  FileRec(f).mode:=fmClosed;
  Move(Name[1],FileRec(f).Name,Length(Name));
End;


Procedure assign(var f:File;p:pchar);
{
  Assign Name to file f so it can be used with the file routines
}
begin
  Assign(f,StrPas(p));
end;


Procedure assign(var f:File;c:char);
{
  Assign Name to file f so it can be used with the file routines
}
begin
  Assign(f,string(c));
end;


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
     Do_Open(f,PChar(@FileRec(f).Name),$1000+filemode);
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
     Do_Open(f,PChar(@FileRec(f).Name),Filemode);
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


Procedure BlockWrite(Var f:File;Var Buf;Count:Longint;var Result:Longint);[IOCheck];
{
  Write Count records from Buf to file f, return written records in result
}
Begin
  Result:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmOutput : ;
  else
    begin
      InOutRes:=103;
      exit;
    end;
  end;
  Result:=Do_Write(FileRec(f).Handle,Longint(@Buf),Count*FileRec(f).RecSize) div FileRec(f).RecSize;
End;


Procedure BlockWrite(Var f:File;Var Buf;Count:Word;var Result:Word);[IOCheck];
{
  Write Count records from Buf to file f, return written records in Result
}
var
  l : longint;
Begin
  BlockWrite(f,Buf,Count,l);
  Result:=l;
End;


Procedure BlockWrite(Var f:File;Var Buf;Count:Word;var Result:Integer);[IOCheck];
{
  Write Count records from Buf to file f, return written records in Result
}
var
  l : longint;
Begin
  BlockWrite(f,Buf,Count,l);
  Result:=l;
End;


Procedure BlockWrite(Var f:File;Var Buf;Count:Longint);[IOCheck];
{
  Write Count records from Buf to file f, if none a Read and Count>0 then
  InOutRes is set
}
var
  Result : Longint;
Begin
  BlockWrite(f,Buf,Count,Result);
  If (Result<Count) and (Count>0) Then
   InOutRes:=101;
End;


Procedure BlockRead(var f:File;var Buf;Count:Longint;var Result:Longint);[IOCheck];
{
  Read Count records from file f ro Buf, return number of read records in
  Result
}
Begin
  Result:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput : ;
  else
    begin
      InOutRes:=103;
      exit;
    end;
  end;
  Result:=Do_Read(FileRec(f).Handle,Longint(@Buf),count*FileRec(f).RecSize) div FileRec(f).RecSize;
End;


Procedure BlockRead(var f:File;var Buf;count:Word;var Result:Word);[IOCheck];
{
  Read Count records from file f to Buf, return number of read records in
  Result
}
var
  l : longint;
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
  l : longint;
Begin
  BlockRead(f,Buf,Count,l);
  Result:=l;
End;


Procedure BlockRead(Var f:File;Var Buf;Count:Longint);[IOCheck];
{
  Read Count records from file f to Buf, if none are read and Count>0 then
  InOutRes is set
}
var
  Result : Longint;
Begin
  BlockRead(f,Buf,Count,Result);
  If (Result<Count) and (Count>0) Then
   InOutRes:=100;
End;


Function FilePos(var f:File):Longint;[IOCheck];
{
  Return current Position In file f in records
}
Begin
  FilePos:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput : ;
  else
    begin
      InOutRes:=103;
      exit;
    end;
  end;
  FilePos:=Do_FilePos(FileRec(f).Handle) div FileRec(f).RecSize;
End;


Function FileSize(var f:File):Longint;[IOCheck];
{
  Return the size of file f in records
}
Begin
  FileSize:=0;
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput : ;
  else
    begin
      InOutRes:=103;
      exit;
    end;
  end;
  if (FileRec(f).RecSize>0) then
   FileSize:=Do_FileSize(FileRec(f).Handle) div FileRec(f).RecSize;
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
    fmInOut,fmInput,fmOutput : ;
  else
    begin
      InOutRes:=103;
      exit;
    end;
  end;
  {Can't use do_ routines because we need record support}
  Eof:=(FileSize(f)<=FilePos(f));
End;


Procedure Seek(var f:File;Pos:Longint);[IOCheck];
{
  Goto record Pos in file f
}
Begin
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput : ;
  else
    begin
      InOutRes:=103;
      exit;
    end;
  end;
  Do_Seek(FileRec(f).Handle,Pos*FileRec(f).RecSize);
End;


Procedure Truncate(Var f:File);[IOCheck];
{
  Truncate/Cut file f at the current record Position
}
Begin
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmOutput : ;
  else
    begin
      InOutRes:=103;
      exit;
    end;
  end;
  Do_Truncate(FileRec(f).Handle,FilePos(f)*FileRec(f).RecSize);
End;


Procedure Close(var f:File);[IOCheck];
{
  Close file f
}
Begin
  If InOutRes <> 0 then
   exit;
  case FileRec(f).Mode of
    fmInOut,fmInput,fmOutput : ;
  else
    begin
      InOutRes:=103;
      exit;
    end;
  end;
  FileRec(f).mode:=fmClosed;
{$IFDEF OS2}
  if (FileRec (F).Handle > 4) or
                            (os_MODE = osOS2) and (FileRec (F).Handle > 2) then
{ Only three standard handles under real OS/2 }
{$ELSE}
  if FileRec(f).Handle>4 then
{$ENDIF}
   Do_Close(FileRec(f).Handle);
End;


Procedure Erase(var f : File);[IOCheck];
Begin
  If InOutRes <> 0 then
   exit;
  If FileRec(f).mode=fmClosed Then
   Do_Erase(PChar(@FileRec(f).Name));
End;


Procedure Rename(var f : File;p:pchar);[IOCheck];
Begin
  If InOutRes <> 0 then
   exit;
  If FileRec(f).mode=fmClosed Then
   Begin
     Do_Rename(PChar(@FileRec(f).Name),p);
     Move(p^,FileRec(f).Name,StrLen(p)+1);
   End;
End;


Procedure Rename(var f : File;const s : string);[IOCheck];
var
  p : array[0..255] Of Char;
Begin
  If InOutRes <> 0 then
   exit;
  Move(s[1],p,Length(s));
  p[Length(s)]:=#0;
  Rename(f,Pchar(@p));
End;


Procedure Rename(var f : File;c : char);[IOCheck];
var
  p : array[0..1] Of Char;
Begin
  If InOutRes <> 0 then
   exit;
  p[0]:=c;
  p[1]:=#0;
  Rename(f,Pchar(@p));
End;

{
  $Log$
  Revision 1.16  2000-01-07 16:41:33  daniel
    * copyright 2000

  Revision 1.15  2000/01/07 16:32:24  daniel
    * copyright 2000 added

  Revision 1.14  1999/10/28 09:52:50  peter
    * use filemode for rewrite instead of mode 1

  Revision 1.13  1999/09/10 15:40:33  peter
    * fixed do_open flags to be > $100, becuase filemode can be upto 255

  Revision 1.12  1999/09/08 16:12:24  peter
    * fixed inoutres for diskfull

  Revision 1.11  1999/09/07 15:54:18  hajny
    * fixed problem with Close under OS/2

  Revision 1.10  1998/11/29 23:10:12  peter
    * also check fmInput,fmOutput

  Revision 1.9  1998/11/29 22:28:11  peter
    + io-error 103 added

  Revision 1.8  1998/09/17 16:34:16  peter
    * new eof,eoln,seekeoln,seekeof
    * speed upgrade for read_string
    * inoutres 104/105 updates for read_* and write_*

  Revision 1.7  1998/09/04 18:16:12  peter
    * uniform filerec/textrec (with recsize:longint and name:0..255)

  Revision 1.6  1998/07/19 19:55:32  michael
  + fixed rename. Changed p to p^

  Revision 1.5  1998/07/02 12:15:39  carl
    + Each IOCheck routine now checks for InOures before executing, like TP

  Revision 1.4  1998/06/23 16:57:16  peter
    * fixed the filesize() problems under linux and filerec.size=0 error

  Revision 1.3  1998/05/21 19:30:56  peter
    * objects compiles for linux
    + assign(pchar), assign(char), rename(pchar), rename(char)
    * fixed read_text_as_array
    + read_text_as_pchar which was not yet in the rtl

  Revision 1.2  1998/05/12 10:42:44  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}
