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
  Recursive;
end.

{
  $Log$
  Revision 1.1  2001-12-10 02:41:41  carl
  + initial version of stack checking routines

}