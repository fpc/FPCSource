{ Source provided for Free Pascal Bug Report 2830 }
{ Submitted by "marco (the gory bugs department)" on  2003-12-04 }
{ e-mail:  }
{$ifdef fpc}{$mode Delphi}{$endif}

Uses SysUtils;

Type
  TFloat=double;
  TCompiledExpression = function: TFloat of object;

procedure bla;
var
  Ce: TCompiledExpression;
begin
  if (TMethod(Ce).Code = TMethod(Ce).Code) and
    (TMethod(Ce).Data = TMethod(Ce).Data) then
     ;
end;

begin
  bla;
end.
