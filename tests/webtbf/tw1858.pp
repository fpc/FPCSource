{ %FAIL }
{ Source provided for Free Pascal Bug Report 1858 }
{ Submitted by "Mattias Gaertner" on  2002-03-07 }
{ e-mail: nc-gaertnma@netcologne.de }
{ expanded for more testing... }
{ Should be tested on systems which detect reads of nil pointers }
unit tw1858;


interface

const

  a : char = '#';
  b = a + 'panic';

  c : set of byte = [0,1,2,3];
  d = c + [4,5,6];
  
implementation

end.

{
  $Log$
  Revision 1.1  2002-03-08 20:18:28  carl
  * moved to correct directory

  Revision 1.2  2002/03/07 21:46:41  carl
  + fix unit name

  Revision 1.1  2002/03/07 21:40:56  carl
  * bug #1858 testing

}  


