{ %norun}
program tw18768;

{$mode delphi}{$H+}

type
  TFoo1 = record
  private
    type
      TFoo3 = record
      private
        b, c: integer;
      strict private
        a: integer;
      public
        function GetFoo2: integer;
      end;
  end;

function TFoo1.TFoo3.GetFoo2: integer;
begin
  c := a * b;
end;

begin
end.
