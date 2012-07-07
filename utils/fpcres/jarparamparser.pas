unit jarparamparser;

{$mode objfpc}{$h+}

interface

uses
  paramparser;

type
  TJarParameters = class(TParameters)
    procedure ParseOutputFormat(aList : TStringList; var index : integer; const parname : string);override;
    procedure ParseArchitecture(aList : TStringList; var index : integer; const parname : string);override;
    procedure ParseSubArchitecture(aList : TStringList; var index : integer; const parname : string);override;
  end;


implementation


{ TJarParameters }

procedure TJarParameters.ParseOutputFormat(aList: TStringList; var index: integer; const parname: string);
begin
  raise EUnknownParameterException.Create(tmp);
end;

procedure TJarParameters.ParseArchitecture(aList: TStringList; var index: integer; const parname: string);
begin
  raise EUnknownParameterException.Create(tmp);
end;

procedure TJarParameters.ParseSubArchitecture(aList: TStringList; var index: integer; const parname: string);
begin
  raise EUnknownParameterException.Create(tmp);
end;

end.
