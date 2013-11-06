{$mode objfpc}

unit ugeneric7;

  interface

    type
      generic tgeneric<t> = class
        l : longint;
        field : t;
        procedure test;
      end;

  implementation

{$R-}
    procedure tgeneric.test;
      begin
        l:=1234;
{$R+}
        field:=l;
{$R-}
        writeln(byte(field));
      end;
{$R+}
end.
