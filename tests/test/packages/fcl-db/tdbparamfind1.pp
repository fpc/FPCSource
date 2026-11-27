{ %skiptarget=msdos,go32v2 }
{ FindParam, and so ParamByName, answers the first parameter of a name, as
  Delphi does; names are compared without case. }
program tdbparamfind1;

{$mode delphi}{$h+}

uses
  SysUtils, DB;

procedure Check(aOk: Boolean; aCode: Integer; const aWhat: string);
begin
  if not aOk then
    begin
      Writeln('failed: ', aWhat);
      Halt(aCode);
    end;
end;

var
  lParams: TParams;
begin
  lParams := TParams.Create;
  try
    lParams.CreateParam(ftInteger, 'Id', ptInput).AsInteger := 1;
    lParams.CreateParam(ftInteger, 'Name', ptInput).AsInteger := 2;
    lParams.CreateParam(ftInteger, 'ID', ptInput).AsInteger := 3;
    Check(lParams.FindParam('id') = lParams[0], 1, 'FindParam answers the first parameter of a name');
    Check(lParams.ParamByName('Id').AsInteger = 1, 2, 'ParamByName reads the first parameter of a name');
    Check(lParams.FindParam('name') = lParams[1], 3, 'FindParam finds a name that occurs once');
    Check(lParams.FindParam('missing') = nil, 4, 'FindParam answers nil for a name not in the list');
  finally
    lParams.Free;
  end;
  Writeln('ok');
end.
