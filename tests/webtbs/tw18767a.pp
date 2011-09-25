{ %norun}
program tw18767a;

{$mode delphi}{$H+}

type
  TFoo = class
  strict private
    const
      n = 3;
    var
      x: array[0..1] of record
        y: array[0..n] of integer;
      end;
  end;

begin
  TFoo.Create;
end.
