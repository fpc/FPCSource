{ %RESULT=202 }
{$ifdef linux}
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
{$endif}
end.

{
  $Log$
  Revision 1.3  2002-03-05 21:54:09  carl
  + indicate error code

  Revision 1.2  2002/01/19 12:37:12  peter
    * no checking for linux

  Revision 1.1  2001/12/10 02:41:41  carl
  + initial version of stack checking routines

}