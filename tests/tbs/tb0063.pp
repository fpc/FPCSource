{ Old file: tbs0069.pp }
{  Shows problem with far qualifier in units            OK 0.99.1 (CEC) }

Unit tb0063;

Interface

Procedure MyTest;Far;         { IMPLEMENTATION expected error. }

{ Further information: NEAR IS NOT ALLOWED IN BORLAND PASCAL  }
{ Therefore the bugfix should only be for the FAR keyword.    }
(* Procedure MySecondTest;Near;                             *)

Implementation

{ near and far are not allowed here, but maybe we don't care since they are ignored by }
{ FPC.                                                                                 }
Procedure MyTest;
Begin
end;

Procedure MySecondTest;
Begin
end;



end.
