{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

unit iostream;

Interface

Uses Classes;

Type

  TiosType = (iosInput,iosOutPut,iosError);
  EIOStreamError = Class(EStreamError);

  TIOStream = Class(THandleStream)
    Private
      FType,
      FPos : Longint;
    Public
      Constructor Create(IOSType : TiosType);
      Function Read(var Buffer; Count: Longint): Longint;override;
      Function Write(const Buffer; Count: Longint): Longint;override;
      Procedure SetSize(NewSize: Longint); override;
      Function Seek(Offset: Longint; Origin: Word): Longint; override;
   end;

Implementation

Const
  SReadOnlyStream = 'Cannot write to an input stream.';
  SWriteOnlyStream = 'Cannot read from an output stream.';
  SInvalidOperation = 'Cannot perform this operation on a IOStream.';

Constructor TIOStream.Create(IOSType : TiosType);

begin
{$ifdef win32}
  Case IOSType of
    iosOutput : FType:=Stdoutputhandle;
    iosInput : FType:=Stdinputhandle;
    iosError : FType:=StdErrorHandle;
  end;
{$else}
  FType:=Ord(IOSType);
{$endif}
  Inherited Create(Ftype);
end;


Function TIOStream.Read(var Buffer; Count: Longint): Longint;

begin
  If Ftype>0 then
    Raise EIOStreamError.Create(SWriteOnlyStream)
  else
    begin
    Result:=Inherited Read(Buffer,Count);
    Inc(FPos,Result);
    end;
end;


Function TIOStream.Write(const Buffer; Count: Longint): Longint;

begin
  If Ftype=0 then
    Raise EIOStreamError.Create(SReadOnlyStream)
  else
    begin
    Result:=Inherited Write(Buffer,Count);
    Inc(FPos,Result);
    end;
end;


Procedure TIOStream.SetSize(NewSize: Longint);

begin
  Raise EIOStreamError.Create(SInvalidOperation);
end;


Function TIOStream.Seek(Offset: Longint; Origin: Word): Longint;

Const BufSize = 100;

Var Buf : array[1..BufSize] of Byte;

begin
  If (Origin=soFromCurrent) and (Offset=0) then
     result:=FPos;
  { Try to fake seek by reading and discarding }
  if (Ftype>0) or
     Not((Origin=soFromCurrent) and (Offset>=0) or
         ((Origin=soFrombeginning) and (OffSet>=FPos))) then
     Raise EIOStreamError.Create(SInvalidOperation);
  if Origin=soFromBeginning then
    Dec(Offset,FPos);
  While ((Offset Div BufSize)>0)
        and (Read(Buf,SizeOf(Buf))=BufSize) do
     Dec(Offset,BufSize);
  If (Offset>0) then
    Read(Buf,BufSize);
  Result:=FPos;
end;

end.

{
  $Log$
  Revision 1.3  2002-09-07 15:15:24  peter
    * old logs removed and tabs fixed

}
