{ %fail }
{ This program should fail compilation, since
  the declaration is not the same as the actual
  implementation, checked against Delphi 3 -
  the class keyword is missing in front
  of procedure tmyclass.myproc
}
{$ifdef fpc}
{$mode delphi}
{$endif}
type
   tmyclass = class
    class procedure myproc;virtual;
   end;

   { missing class keyword in front of procedure }
   procedure tmyclass.myproc;
    begin
    end;


Begin
end.
