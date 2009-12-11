{ %fail }

program project1;
{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

type
  IIntf1 = interface['{484A7AC5-114E-4D99-9E4F-7B4906413B2F}'] end;

  IIntf2 = interface['{3718A1FC-A6F6-4465-965D-14FF1CBA1902}']
    procedure Print2;
  end;

  TClass2 = class(TInterfacedObject, IIntf2)
    procedure Print2;
  end;

  TClass1 = class(TInterfacedObject, IIntf1, IIntf2)
    private
      FIntf2:IIntf2;
      function GetIntf2:IIntf2;cdecl; // <--- should be forbidden
    public
      constructor Create;
      property I:IIntf2 read GetIntf2 implements IIntf2;
  end;


procedure TClass2.Print2;
begin
  Writeln('doing something');
end;

function TClass1.GetIntf2: IIntf2;stdcall;
begin
  Result:=FIntf2;
end;

constructor TClass1.Create;
begin
  inherited Create;
  FIntf2:=TClass2.Create;
end;

begin
end.
