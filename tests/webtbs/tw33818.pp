{ %OPT=-Seh -vh }
{ %norun }
{$mode objfpc}

program Project1;

uses
 SysUtils;

procedure RaiseException(AReturnAddress : CodePointer);
begin
 raise Exception.Create('message') at AReturnAddress;
end;

procedure RaiseException(AReturnAddress : CodePointer;AFrame : Pointer);
begin
 raise Exception.Create('message') at AReturnAddress,AFrame;
end;

begin
 RaiseException(CodePointer(10));
 RaiseException(CodePointer(10),Pointer(1234));
end.
