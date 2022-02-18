{ %NORUN }

program tw39581;

{$Mode Delphi} {$H+}

Type
  TImplClass<P> = class;

  iLinkingIntf<P> = interface
    procedure NestedCall(const DataFrom: TImplClass<P>);
  end;

(* Компиляция проекта, цель: Project1.exe: Код завершения 1, ошибок: 1
Project1.pas(9,55) Error: Internal error 2012101001
*)

  { TImplClass }

  TImplClass<P> = class( TInterfacedObject, iLinkingIntf<P> )
  protected
    procedure NestedCall(const DataFrom: TImplClass<P> );
  end;

{ TImplClass }

procedure TImplClass<P>.NestedCall(const DataFrom: TImplClass<P>);
begin

end;


begin
end.

