{
    $Id: idea.pp,v 1.7 2005/02/14 17:13:15 peter Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef fpc}
{$mode objfpc}
{$endif}

UNIT IDEA;

{
 IDEA encryption routines for pascal
 ported from PGP 2.3

 IDEA encryption routines for pascal, ported from PGP 2.3
 Copyright (C) for this port 1998 Ingo Korb
 Copyright (C) for the stream support 1999 Michael Van Canneyt

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Library General Public
 License as published by the Free Software Foundation; either
 version 2 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Library General Public License for more details.

 You should have received a copy of the GNU Library General Public
 License along with this library; if not, write to the Free
 Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}


{$R-,Q-}
{ Not nice but fast... }

INTERFACE

Uses Sysutils,Classes;

CONST IDEAKEYSIZE = 16;
      IDEABLOCKSIZE = 8;
      ROUNDS = 8;
      KEYLEN = (6*ROUNDS+4);

TYPE IDEAkey = ARRAY[0..keylen-1] OF Word;
     ideacryptkey = ARRAY[0..7] OF Word;
     ideacryptdata = ARRAY[0..3] OF Word;

PROCEDURE EnKeyIdea(userkey: ideacryptkey; VAR z: ideakey);
PROCEDURE DeKeyIdea(z: IDEAKey; VAR dk: ideakey);
PROCEDURE CipherIdea(input: ideacryptdata; VAR outdata: ideacryptdata; z: IDEAkey);

Type

EIDEAError = Class(EStreamError);

TIDEAEncryptStream = Class(TStream)
  private
    FDest : TStream;
    FKey : IDEAKey;
    FData : IDEACryptData;
    FBufpos : Byte;
    FPos : Longint;
  public
    constructor Create(AKey : ideakey; Dest: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Flush;
    Property Key : IDEAKey Read FKey;
  end;

TIDEADeCryptStream = Class(TStream)
  private
    FSRC : TStream;
    FKey : IDEAKey;
    FData : IDEACryptData;
    FBufpos : Byte;
    FPos : Longint;
  public
    constructor Create(AKey : ideakey; Src: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    Property Key : IDEAKey Read FKey;
  end;

IMPLEMENTATION

Const
  SNoSeekAllowed = 'Seek not allowed on encryption streams';
  SNoReadAllowed = 'Reading from encryption stream not allowed';
  SNoWriteAllowed = 'Writing to decryption stream not allowed';

{$ifdef fpc}
Type
  PChar = ^Byte;
{$endif}

PROCEDURE mul(VAR a:Word; b: Word);
VAR p: LongInt;
BEGIN
  IF (a <> 0) THEN BEGIN
    IF (b <> 0) THEN BEGIN
      p := LongInt(a)*b;
      b := p;
      a := p SHR 16;
      IF (b < a) THEN a := b - a + 1
                 ELSE a := b - a;
    END ELSE a := 1 - a;
  END ELSE a := 1-b;
END;

FUNCTION inv(x: word): Word;
VAR t0,t1,q,y: Word;
BEGIN
  IF x <= 1 THEN BEGIN
    inv := x;
    exit;
  END;
  t1 := 65537 DIV x;
  y := 65537 MOD x;
  IF y = 1 THEN BEGIN
    inv := Word(1-t1);
    exit;
  END;
  t0 := 1;
  REPEAT
    q := x DIV y;
    x := x MOD y;
    t0 := t0 + q * t1;
    IF x = 1 THEN BEGIN
      inv := t0;
      exit;
    END;
    q := y DIV x;
    y := y MOD x;
    t1 := t1 + q*t0;
  UNTIL y = 1;
  inv := word(1-t1);
END;

PROCEDURE EnKeyIdea(userkey: ideacryptkey; VAR z: ideakey);
VAR zi,i,j: integer;
BEGIN
  FOR j := 0 TO 7 DO z[j] := userkey[j];
  zi := 0;
  i := 0;
  FOR j := 8 TO keylen-1 DO BEGIN
    Inc(i);
    z[zi+i+7] := (z[zi+(i AND 7)] SHL 9) OR (z[zi+((i+1) AND 7)] SHR 7);
    zi := zi + (i AND 8);
    i := i AND 7;
  END;
  FOR i := 0 TO 7 DO userkey[i] := 0;
END;

PROCEDURE DeKeyIdea(z: IDEAKey; VAR dk: ideakey);
VAR j: Integer;
    t1,t2,t3: Word;
    p: IDEAKey;
    ip: Integer;
    iz: Integer;
BEGIN
  iz := 0;
  ip := keylen;
  FOR j := 0 TO keylen - 1 DO p[j] := 0;
  t1 := inv(z[iz]);   Inc(iz);
  t2 := not(z[iz])+1; Inc(iz);
  t3 := not(z[iz])+1; Inc(iz);
  Dec(ip); p[ip] := inv(z[iz]); Inc(iz);
  Dec(ip); p[ip] := t3;
  Dec(ip); p[ip] := t2;
  Dec(ip); p[ip] := t1;
  FOR j := 1 TO rounds-1 DO BEGIN
    t1 := z[iz]; Inc(iz);
    Dec(ip); p[ip] := z[iz]; Inc(iz);
    Dec(ip); p[ip] := t1;
    t1 := inv(z[iz]);   Inc(iz);
    t2 := Not(z[iz])+1; Inc(iz);
    t3 := Not(z[iz])+1; Inc(iz);
    Dec(ip); p[ip] := inv(z[iz]); Inc(iz);
    Dec(ip); p[ip] := t2;
    Dec(ip); p[ip] := t3;
    Dec(ip); p[ip] := t1;
  END;
  t1 := z[iz]; Inc(iz);
  Dec(ip); p[ip] := z[iz]; Inc(iz);
  Dec(ip); p[ip] := t1;
  t1 := inv(z[iz]);   Inc(iz);
  t2 := Not(z[iz])+1; Inc(iz);
  t3 := Not(z[iz])+1; Inc(iz);
  Dec(ip); p[ip] := inv(z[iz]);
  Dec(ip); p[ip] := t3;
  Dec(ip); p[ip] := t2;
  Dec(ip); p[ip] := t1;
  FOR j := 0 TO KeyLen-1 DO BEGIN
    dk[j] := p[j];
    p[j] := 0;
  END;
  FOR j := 0 TO 51 DO z[j] := 0;
END;

PROCEDURE CipherIdea(input: ideacryptdata; VAR outdata: ideacryptdata; z:IDEAkey);
VAR x1, x2, x3, x4, t1, t2: Word;
    r: Integer;
    zi: Integer;
BEGIN
  zi := 0;
  x1 := input[0];
  x2 := input[1];
  x3 := input[2];
  x4 := input[3];
  FOR r := 1 TO ROUNDS DO BEGIN
    mul(x1,z[zi]);    Inc(zi);
    x2 := x2 + z[zi]; Inc(zi);
    x3 := x3 + z[zi]; Inc(zi);
    mul(x4, z[zi]);   Inc(zi);
    t2 := x1 XOR x3;
    mul(t2, z[zi]);   Inc(zi);
    t1 := t2 + (x2 XOR x4);
    mul(t1, z[zi]);   Inc(zi);
    t2 := t1+t2;
    x1 := x1 XOR t1;
    x4 := x4 XOR t2;
    t2 := t2 XOR x2;
    x2 := x3 XOR t1;
    x3 := t2;
  END;
  mul(x1, z[zi]);       Inc(zi);
  outdata[0] := x1;
  outdata[1] := x3 + z[zi]; Inc(zi);
  outdata[2] := x2 + z[zi]; Inc(zi);
  Mul(x4,z[zi]);
  outdata[3] := x4;
  FOR r := 0 TO 3 DO input[r] := 0;
  FOR r := 0 TO 51 DO z[r] := 0;
END;

constructor TIDEAEncryptStream.Create(AKey : ideakey; Dest: TStream);

begin
  inherited Create;
  FKey:=AKey;
  FDest:=Dest;
  FBufPos:=0;
  Fpos:=0;
end;

Destructor TIDEAEncryptStream.Destroy;


begin
  Flush;
  Inherited Destroy;
end;

Procedure TIDEAEncryptStream.Flush;

Var
  OutData : IdeaCryptData;

begin
  If FBufPos>0 then
    begin
    // Fill with nulls
    FillChar(PChar(@FData)[FBufPos],SizeOf(FData)-FBufPos,#0);
    CipherIdea(Fdata,OutData,FKey);
    FDest.Write(OutData,SizeOf(OutData));
    // fixed: Manual flush and then free will now work
    FBufPos := 0;
    end;
end;

function TIDEAEncryptStream.Read(var Buffer; Count: Longint): Longint;

begin
  Raise EIDEAError.Create(SNoReadAllowed);
end;

function TIDEAEncryptStream.Write(const Buffer; Count: Longint): Longint;

Var
  mvsize : Longint;
  OutData : IDEAcryptdata;

begin
  Result:=0;
  While Count>0 do
    begin
    MVsize:=Count;
    If Mvsize>SizeOf(Fdata)-FBufPos then
      mvsize:=SizeOf(FData)-FBufPos;
    Move(PChar(@Buffer)[Result],PChar(@FData)[FBufPos],MVSize);
    If FBufPos+mvSize=Sizeof(FData) then
      begin
      // Empty buffer.
      CipherIdea(Fdata,OutData,FKey);
      // this will raise an exception if needed.
      FDest.Writebuffer(OutData,SizeOf(OutData));
      FBufPos:=0;
      end
    else
      inc(FBufPos,mvsize);
    Dec(Count,MvSize);
    Inc(Result,mvSize);
    end;
  Inc(FPos,Result);
end;


function TIDEAEncryptStream.Seek(Offset: Longint; Origin: Word): Longint;

begin
  if (Offset = 0) and (Origin = soFromCurrent) then
    Result := FPos
  else
    Raise EIDEAError.Create(SNoSeekAllowed);
end;

constructor TIDEADeCryptStream.Create(AKey : ideakey; Src: TStream);

begin
  inherited Create;
  FKey:=AKey;
  FPos:=0;
  FBufPos:=SizeOf(Fdata);
  FSrc:=Src;
end;

destructor TIDEADeCryptStream.Destroy;
begin
  Inherited destroy;
end;

function TIDEADeCryptStream.Read(var Buffer; Count: Longint): Longint;

Var
  mvsize : Longint;
  InData : IDEAcryptdata;

begin
  Result:=0;
  While Count>0 do
    begin
    // Empty existing buffer.
    If FBufPos<SizeOf(FData) then
      begin
      mvSize:=Sizeof(FData)-FBufPos;
      If MvSize>count then
        mvsize:=Count;
      Move(PChar(@FData)[FBufPos],PChar(@Buffer)[Result],MVSize);
      Dec(Count,mvsize);
      Inc(Result,mvsize);
      inc(fBufPos,mvsize);
      end;
    // Fill buffer again if needed.
    If (FBufPos=SizeOf(FData)) and (Count>0) then
      begin
      mvsize:=FSrc.Read(InData,SizeOf(InData));
      If mvsize>0 then
        begin
        If MvSize<SizeOf(InData) Then
          // Fill with nulls
          FillChar(PChar(@InData)[mvsize],SizeOf(InData)-mvsize,#0);
        CipherIdea(InData,FData,FKey);
        FBufPos:=0;
        end
      else
        Count:=0; // No more data available from stream; st
      end;
    end;
  Inc(FPos,Result);
end;

function TIDEADeCryptStream.Write(const Buffer; Count: Longint): Longint;
begin
  Raise EIDEAError.Create(SNoWriteAllowed);
end;

function TIDEADeCryptStream.Seek(Offset: Longint; Origin: Word): Longint;

Var Buffer : Array[0..1023] of byte;
    i : longint;

begin
  // Fake seek if possible by reading and discarding bytes.
  If ((Offset>=0) and (Origin = soFromCurrent)) or
    ((Offset>FPos) and (Origin = soFromBeginning)) then
      begin
      For I:=1 to (Offset div SizeOf(Buffer)) do
        ReadBuffer(Buffer,SizeOf(Buffer));
      ReadBuffer(Buffer,Offset mod SizeOf(Buffer));
      Result:=FPos;
      end
  else
    Raise EIDEAError.Create(SNoSeekAllowed);
end;

END.

{
  $Log: idea.pp,v $
  Revision 1.7  2005/02/14 17:13:15  peter
    * truncate log

}
