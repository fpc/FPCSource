Program Example26;

{ This program demonstrates the CreateDir and RemoveDir functions }
{ Run this program twice in the same directory }

Uses sysutils;

Begin
  If Not DirectoryExists('NewDir') then
    If Not CreateDir ('NewDir') Then
      Writeln ('Failed to create directory !')
    else
      Writeln ('Created "NewDir" directory')
  Else
    If Not RemoveDir ('NewDir') Then
      Writeln ('Failed to remove directory !')
    else
      Writeln ('Removed "NewDir" directory');
End.