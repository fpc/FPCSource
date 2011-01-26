program tw18610;

{$mode delphi}{$H+}

type
  IInt = interface
    procedure Test;
  end;

  TParent = class
  private
    type
      TChild = class(TInterfacedObject, IInt)
      public
        procedure Test;
      end;
  end;

procedure TParent.TChild.Test;
begin
end;


begin
end.
