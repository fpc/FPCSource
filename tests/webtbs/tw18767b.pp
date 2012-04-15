{ %norun}
program tw18767b;

{$mode delphi}{$H+}

type
  TFoo = class
  strict private
    type
      TBar = (one, two);
    var
      x: array of record
        y: array[TBar] of integer;
      end;
  end;

begin
  TFoo.Create;
end.
