{ Old file: tbs0278.pp }
{ (* in conditional code is handled wrong for tp,delphi OK 0.99.13 (PFV) }

{$ifdef fpc}{$mode tp}{$endif}
unit tb0238;

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
