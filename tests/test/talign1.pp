{ This verifies if the resource string table
  is correctly aligned, normally the assembler
  should be verified manually.
}
unit talign1;

{$mode objfpc}

interface

resourcestring
  First = 'This is is a small test of a unit for resource strings';
  Second = 'This is also a small test';

implementation

end.
{
   $Log: talign1.pp,v $
   Revision 1.2  2005/02/14 17:13:35  peter
     * truncate log

}
