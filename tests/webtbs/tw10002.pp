program OverloadMistaken;

{$ifdef fpc}
{$mode delphi}
{$endif}

type _ulong = Cardinal;

TCCC = class
public
    constructor Create(Size: _ulong=0); overload;
    constructor Create(Buffer: Pointer); overload;
end;

constructor TCCC.Create(Size: _ulong);
begin
	inherited Create;
	WriteLn('TCCC.Create(Size: _ulong) called.');
end;

constructor TCCC.Create(Buffer: Pointer);
begin
 halt(1);
end;

var c: TCCC;
l: longint;
begin
	c := TCCC.Create(20);
	c := TCCC.Create(l);
end.
