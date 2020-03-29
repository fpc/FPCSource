{$MODE DELPHI}

program CharOverload;

uses
  SysUtils;

procedure Foo(const aArg: UnicodeString); overload;
begin
  WriteLn('WideString: ', aArg);
end;

procedure Foo(c: WideChar); overload;
begin
  WriteLn('Char: ', c);
end;

begin
  Foo('abc');
end.
