{ %fail }

{ Source provided for Free Pascal Bug Report 3331 }
{ Submitted by "Ales Katona" on  2004-09-26 }
{ e-mail: ales@chello.sk }

{$mode delphi}

type TA = class
      private
       a: cardinal;
       b: cardinal;
       procedure doit;
     end;

procedure TA.doit;
begin
  // This should not be allowed
 for a:=0 to 2 do
  b:=0;
end;

begin
end.
