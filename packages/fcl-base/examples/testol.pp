program testol;

{$mode objfpc}

uses sysutils,contnrs;

Type
  TTestObject = Class(TObject)
    FID : Integer;
    Fmsg : String;
    Constructor Create (ID : INteger; Msg : String);
    Destructor Destroy; override;
    Procedure Print;
  end;

Constructor TTestObject.Create (ID : INteger; Msg : String);

begin
  FID:=ID;
  FMsg:=Msg;
end;

procedure TTestObject.Print;

begin
  Writeln(FID,' (',FMsg,')');
end;

Destructor TTEstObject.Destroy;

begin
  Write('Destroying : ');
  Print;
end;

Const
  NO = 10;

Var
  S : TObjectList;
  I : Integer;

begin
  S:=TobjectList.Create;
  For I:=1 to NO do
    S.add(TTestObject.Create(I,'Item '+intToStr(i)));
  Writeln('Counting objects:');
  For I:=0 to S.Count-1 do
    (S[i] as TTestObject).Print;
  Writeln('Shifting..');
  For I:=1 to 5 do
    S.Move(9,0);
  Writeln('Counting objects:');
  For I:=0 to S.Count-1 do
    (S[i] as TTestObject).Print;
  Writeln('Deleting objects:');
  For I:=S.Count-1 downto 0 do
    S.Delete(I);
end.
