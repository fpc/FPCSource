// $Id$

{$MODE objfpc}
{$H+}

program cfgtest;
uses xmlcfg;
var
  cfg: TXMLConfig;
begin

  WriteLn('Writing a sample XML configuration to "testcfg.xml"...');

  cfg := TXMLConfig.Create('testcfg.xml');
  cfg.SetValue('cfgtest/MainWindow/Constraints/Width', 600);
  cfg.SetValue('cfgtest/MainWindow/Constraints/Height', 400);
  cfg.SetValue('cfgtest/MainWindow/Caption', 'TXMLConfig Test');
  cfg.SetValue('cfgtest/SomeForm/Presets/Preset1/Name', 'Example');
  cfg.SetValue('TipOfTheDay/Disabled', True);
  cfg.Free;

  WriteLn('Ok; now I''ll try to read back all values...');

  cfg := TXMLConfig.Create('testcfg.xml');
  if cfg.GetValue('cfgtest/MainWindow/Constraints/Width', 0) <> 600 then
    WriteLn('Invalid value: cfgtest/MainWindow/Constraints/Width');
  if cfg.GetValue('cfgtest/MainWindow/Constraints/Height', 400) <> 400 then
    WriteLn('Invalid value: cfgtest/MainWindow/Constraints/Height');
  if cfg.GetValue('cfgtest/MainWindow/Caption', '') <> 'TXMLConfig Test' then
    WriteLn('Invalid value: cfgtest/MainWindow/Caption');
  if cfg.GetValue('cfgtest/SomeForm/Presets/Preset1/Name', '') <> 'Example' then
    WriteLn('Invalid value: cfgtest/SomeForm/Presets/Preset1/Name');
  if cfg.GetValue('TipOfTheDay/Disabled', False) <> True then
    WriteLn('Invalid value: TipOfTheDay/Disabled');
  cfg.Free;

  WriteLn('Done!');
end.


{
  $Log$
  Revision 1.1  1999-07-09 21:06:59  michael
  + Initial implementation by sebastian Guenther

}
