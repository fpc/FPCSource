{$mode objfpc}

unit ugeneric7;

  interface

    type
      generic tgeneric<t> = class
        field : t;
        procedure test;
      end;

  implementation

{$R-}
    procedure tgeneric.test;
      var
        l : longint;
      begin
        l:=1234;
{$R+}
        field:=l;
{$R-}
        writeln(byte(field));
      end;
{$R+}
end.
