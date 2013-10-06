{ %RESULT=202 }
{ %OPT=-O- }
{ do not optimize because the code below would cause an endless loop due to tail recursion elimination }

{$ifdef unix}
  {$define nocheck}
{$endif}

{$S+}

{ This tests the stack checking routine on those }
{ targets which support it.                      }

procedure recursive;
 var s: string;
  begin
    s := 'blahblah';
    recursive;
  end;


Begin
{$ifndef nocheck}
  Recursive;
{$else}
  { Simulate the correct error code }
  RunError(202);
{$endif}
end.
