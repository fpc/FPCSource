{ %fail }
{ Returns this error under Delphi :
  Error: Types of actual and formal var parameters must be identical }
{$ifdef fpc}
{$mode delphi}
{$endif}
type
  tsymbol = class
  end;

  tderivedsymbol = class(tsymbol)
  end;



procedure testclass(var t: tsymbol);
begin
end;

var
 myclass : tderivedsymbol;
begin
 myclass  := tderivedsymbol.create;
 testclass(myclass);
end.
