program tforin2;

// test operator Enumerator for the string type and built-in into RTL TStrings enumerator

{$mode objfpc}{$H+}
{$apptype console}
uses
  Classes;

type
  { TStringEnumerator }

  TStringEnumerator = class // reverse enumerator
  private
    FString: String;
    FIndex: Integer;
    function GetCurrent: Char; inline;
  public
    constructor Create(AString: String);
    function MoveNext: Boolean;
    property Current: Char read GetCurrent;
  end;

{ TStringEnumerator }

function TStringEnumerator.GetCurrent: Char;
begin
  Result := FString[FIndex];
end;

constructor TStringEnumerator.Create(AString: String);
begin
  inherited Create;
  FString := AString;
  FIndex := Length(FString) + 1;
end;

function TStringEnumerator.MoveNext: Boolean;
begin
  dec(FIndex);
  Result := FIndex > 0;
end;

// define an operator for the string type
operator enumerator (const s: string): TStringEnumerator;inline;
begin
  Result := TStringEnumerator.Create(s);
end;

procedure LoopString(s: string);

  function getstring(s: string): string;
  begin
    result:=s;
  end;

var
  c: char;
begin
  // check loop in string. output is 'tset' for the string 'test' :)
  for c in getstring(s) do
    write(c);
  WriteLn;
end;

procedure LoopClass;
var
  L: TStringList;
  S: String;
begin
  L := TStringList.Create;
  L.Add('1');
  L.Add('2');
  L.Add('3');
  for S in L do
    WriteLn(S);
  L.Free;
end;

begin
  LoopString('test');
  LoopClass;
end.

