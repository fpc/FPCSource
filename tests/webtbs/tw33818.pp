{ %OPT=-Seh -vh }
{ %norun }
{$mode objfpc}

program Project1;

uses
 SysUtils;

procedure RaiseException(AReturnAddress : Pointer);
begin
 raise Exception.Create('message') at AReturnAddress;
end;

procedure RaiseException(AReturnAddress : Pointer;AFrame : Pointer);
begin
 raise Exception.Create('message') at AReturnAddress,AFrame;
end;

begin
 RaiseException(Pointer(10));
 RaiseException(Pointer(10),Pointer(1234));
end.
