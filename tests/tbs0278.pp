{$ifdef fpc}{$mode tp}{$endif}
unit tbs0278;

interface

{
a string constant within $IFDEF that
contains "(*" causes an error;
compile it with "ppc386 test -So"  or  "-Sd"
}

var
  c : char;

{$IFDEF not_defined}
const
   c = 'b''(*

{ $else}

var
  c : char;

{$ENDIF}


implementation

end.
