{ %norun }

{$mode delphi}

type
  tc = class
    type
      tforward = class;

      tnested = class
        type
          tforward = class;

          tforward = class
          end;
      end;

      tforward = class
      end;
  end;

begin
end.
