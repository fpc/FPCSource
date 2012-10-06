{$ifdef fpc}
{$mode delphi}
{$endif}

unit usprot1;

interface

type
  tbase = class
   strict protected
    procedure pmethod; virtual; overload;
  end;

implementation

procedure tbase.pmethod;
begin
end;

end.
