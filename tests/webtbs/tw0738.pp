{$mode delphi}

type
 (*
 {$IFDEF FPC}
 SomeClass = class; { this line shouldn't be necessary }
 {$ENDIF}
 *)

 SomeClass = class
  SomeMember:SomeClass;
 end;

begin
end.
