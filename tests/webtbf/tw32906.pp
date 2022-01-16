{ %cpu=i386 }
{ %fail }

program Project1;

{$mode delphi}
{$ASMMODE INTEL}

type
  TA = class
    b: integer;
  end;

procedure Test();
  var
    a: TA;
    r: integer;

  begin
    a := TA.Create();
    a.b := 5;

    asm
      CMP a.b, 7
      JG @Bigger
      MOV r, 1
      JMP @Exit
      @Bigger:
      MOV r, 2
      @Exit:
    end [];

    writeln(r);
    readln();
end;

begin
  Test();
end.
