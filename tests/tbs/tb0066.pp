{ Old file: tbs0073.pp }
{  shows incompatiblity with bp for distance qualifiers OK 0.99.6 (PFV) }

Unit tb0066;

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



Procedure MySecondTest;Far;
Begin
end;





end.
