{$mode delphi}

type
 (*
 {$IFDEF FPK}
 SomeClass = class; { this line shouldn't be necessary }
 {$ENDIF}
 *)

 SomeClass = class
  SomeMember:SomeClass;
 end;

begin
end.
