Program Example53;

{ Program to demonstrate the MkDir and RmDir functions. }

Const D : String[8] = 'TEST.DIR';

Var S : String;

begin
  Writeln ('Making directory ',D);
  Mkdir (D);
  Writeln ('Changing directory to ',D);
  ChDir (D);
  GetDir (0,S);
  Writeln ('Current Directory is : ',S);
  WRiteln ('Going back');
  ChDir ('..');
  Writeln ('Removing directory ',D);
  RmDir (D);
end.
