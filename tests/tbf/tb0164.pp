{ %fail }

program tb0164;

{ Non local goto cannot be handled properly by the compiler,
  and should therefore not be allowed.}

{$GOTO ON}

  procedure foo;

    label 999;

    procedure bar;

    begin
      goto 999
    end;

  begin
    bar;
    999:
  end;

begin
  foo;
end.
