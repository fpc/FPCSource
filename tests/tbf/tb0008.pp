{ %FAIL }
{ Old file: tbf0075.pp }
{  shows invalid pchar output to console                 OK 0.99.1 }

Unit tbs0075;

Interface


Procedure MyTest;Far;         { IMPLEMENTATION expected error. }

{ Further information: NEAR IS NOT ALLOWED IN BORLAND PASCAL  }
{ Therefore the bugfix should only be for the FAR keyword.    }
 Procedure MySecondTest;

Implementation

{ near and far are not allowed here, but maybe we don't care since they are ignored by }
{ FPC.                                                                                 }
Procedure MyTest;
Begin
end;

Procedure MySecondTest;Far;Forward;


Procedure MySecondTest;Far;
Begin
end;





end.
