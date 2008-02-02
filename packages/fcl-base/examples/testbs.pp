{
    This file is part of the Free Component Library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Test for TBufstream.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
program testbs;


uses
  Classes, SysUtils
  { add your units here }, bufstream;

Var
  MBSize     : Integer = 1024*100;
  SBCapacity : Integer = 1024*16;

procedure TestRead(Buffer : PChar; ACapacity : Integer);

Var
  F2 : TFileStream;
  B : TReadBufSTream;
  C : Integer;

begin
  B:=TReadBufStream.Create(TFileStream.Create(PAramStr(0),fmOpenRead),ACapacity);
  Try
    B.SourceOwner:=True;
    F2:=TFileStream.Create(ChangeFileExt(PAramStr(0),'.tr'),fmCreate);
    Try
      Repeat
        C:=B.Read(Buffer^,MBSize);
        F2.Write(Buffer^,C);
      Until (C<MBSize);
    Finally
      F2.Free;
    end;
  finally
    B.Free;
  end;
end;

procedure TestWrite(Buffer : PChar; ACapacity : Integer);

Var
  F : TFileStream;
  B : TWriteBufSTream;
  C : Integer;

begin
  F:=TFileStream.Create(PAramStr(0),fmOpenRead);
  Try
    B:=TWriteBufStream.Create(TFileStream.Create(ChangeFileExt(PAramStr(0),'.tw'),fmCreate),ACapacity);
    Try
      B.SourceOwner:=True;
      Repeat
        C:=F.Read(Buffer^,MBSize);
        B.Write(Buffer^,C);
      Until (C<MBSize);
    Finally
      B.Free;
    end;
  finally
    F.Free;
  end;
end;

Var
  Buffer : PChar;

begin
  If ParamCount>0 then
    MBSize:=StrToIntDef(ParamStr(1),MBSize);
  If ParamCount>1 then
    SBCapacity:=StrToIntDef(ParamStr(2),SBCapacity);
  GetMem(Buffer,MBSize);
  Try
    TestRead(Buffer,SBCapacity);
    TestWrite(Buffer,SBCapacity);
  Finally
    FreeMem(Buffer);
  end;
end.
