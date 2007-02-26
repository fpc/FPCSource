{ %norun }
{ %target=win32,wince }
{ %opt=-Aas }

Procedure InternalName; CDecl; External 'thedll' name '?ExternalName@Tralala';

Begin
  InternalName;
End.
