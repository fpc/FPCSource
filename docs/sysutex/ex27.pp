Program Example27;

{ This program demonstrates the DiskFree function }

Uses sysutils;

Begin
  Write ('Size of current disk       : ',DiskSize(0));
  Writeln (' (= ',DiskSize(0) div 1024,'k)');
  Write ('Free space of current disk : ',Diskfree(0));
  Writeln (' (= ',Diskfree(0) div 1024,'k)');
End.