{**************************
     Port IO functions
***************************}

Function  IOperm (From,Num : Cardinal; Value : Longint) : boolean;
{$ifndef BSD}
Function  IoPL(Level : longint) : Boolean;
{$endif}
