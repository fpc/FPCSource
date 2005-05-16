{ %RESULT=202 }
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

{
  $Log: teststk.pp,v $
  Revision 1.7  2005/02/14 17:13:37  peter
    * truncate log

}
