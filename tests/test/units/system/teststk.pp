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
  $Log$
  Revision 1.5  2002-06-01 19:08:52  marco
   * Renamefest

  Revision 1.4  2002/03/09 23:18:51  carl
  * simulate the error code on system with no stack checking

  Revision 1.3  2002/03/05 21:54:09  carl
  + indicate error code

  Revision 1.2  2002/01/19 12:37:12  peter
    * no checking for linux

  Revision 1.1  2001/12/10 02:41:41  carl
  + initial version of stack checking routines

}