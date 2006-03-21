{$mode delphi}

type
  TStream = pointer;
  EncodingMemoryProc = function ( pIN, pOUT: PByte; Size: integer; MIMELine: integer = 0): integer;
  DecodingMemoryProc = function ( pIN, pOUT: PByte; Size: integer): integer;
var
  err : boolean;

function SZFullEncodeBase64(sIN, sOUT: TStream; Size: integer=-1; MIMELine: integer = 0): integer; overload;
begin
end;

function SZFullEncodeBase64(pIN, pOUT: PByte; Size: integer; MIMELine: integer = 0): integer; overload;
begin
  err:=false;
  writeln('success');
end;

function SZDecodeBase64(pIN, pOUT: PByte; Size: integer): integer; overload;
begin
end;

function SZDecodeBase64(sIN, sOUT: TStream): integer; overload;
begin
end;

function DoEncodingMemory( Encoding: EncodingMemoryProc; Decoding: DecodingMemoryProc): integer;
begin
  Encoding(nil,nil,0);
end;


begin
  err:=true;
  DoEncodingMemory(
    SZFullEncodeBase64,
    SZDecodeBase64);
  if err then
    begin
      writeln('Error!');
      halt(1);
    end;
end.

