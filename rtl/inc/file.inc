{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

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


Procedure Rewrite(var f:File;l:Word);[IOCheck];
{
  Create file f with recordsize of l
}
Begin
  If l=0 Then
   InOutRes:=2
  else
   Begin
     Do_Open(f,PChar(@FileRec(f).Name),$101);
     FileRec(f).RecSize:=l;
   End;
End;


Procedure Reset(var f:File;l:Word);[IOCheck];
{
  Open file f with recordsize of l and filemode
}
Begin
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
  Rewrite(f,128);
End;


Procedure Reset(Var f:File);[IOCheck];
{
  Open file with (default) 128 byte records
}
Begin
  Reset(f,128);
End;


Procedure BlockWrite(Var f:File;Var Buf;Count:Longint;var Result:Longint);[IOCheck];
{
  Write Count records from Buf to file f, return written records in result
}
Begin
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
  If (Result=0) and (Count>0) Then
   InOutRes:=101;
End;


Procedure BlockRead(var f:File;var Buf;Count:Longint;var Result:Longint);[IOCheck];
{
  Read Count records from file f ro Buf, return nuùber of read records in
  Result
}
Begin
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
  If (Result=0) and (Count>0) Then
   InOutRes:=100;
End;


Function FilePos(var f:File):Longint;[IOCheck];
{
  Return current Position In file f in records
}
Begin
  FilePos:=Do_FilePos(FileRec(f).Handle) div FileRec(f).RecSize;
End;


Function FileSize(var f:File):Longint;[IOCheck];
{
  Return the size of file f in records
}
Begin
  FileSize:=Do_FileSize(FileRec(f).Handle) div FileRec(f).RecSize;
End;


Function Eof(var f:File):Boolean;[IOCheck];
{
  Return True if we're at the end of the file f, else False is returned
}
Begin
  {Can't use do_ routines because we need record support}
  Eof:=(FileSize(f)<=FilePos(f));
End;


Procedure Seek(var f:File;Pos:Longint);[IOCheck];
{
  Goto record Pos in file f
}
Begin
  Do_Seek(FileRec(f).Handle,Pos*FileRec(f).RecSize);
End;


Procedure Truncate(Var f:File);[IOCheck];
{
  Truncate/Cut file f at the current record Position
}
Begin
  Do_Truncate(FileRec(f).Handle,FilePos(f)*FileRec(f).RecSize);
End;


Procedure Close(var f:File);[IOCheck];
{
  Close file f
}
Begin
  If (FileRec(f).mode<>fmClosed) Then
   Begin
     FileRec(f).mode:=fmClosed;
     Do_Close(FileRec(f).Handle);
   End;
End;


Procedure Erase(var f : File);[IOCheck];
Begin
  If FileRec(f).mode=fmClosed Then
   Do_Erase(PChar(@FileRec(f).Name));
End;


Procedure Rename(var f : File;const s : string);[IOCheck];
var
  p : array[0..255] Of Char;
Begin
  If FileRec(f).mode=fmClosed Then
   Begin
     Move(s[1],p,Length(s));
     p[Length(s)]:=#0;
     Do_Rename(PChar(@FileRec(f).Name),PChar(@p));
     Move(p,FileRec(f).Name,Length(s)+1);
   End;
End;

{
  $Log$
  Revision 1.2  1998-05-12 10:42:44  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}
