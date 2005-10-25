
{$MODE objfpc}
{$H+}

program cfgtest;
uses xmlcfg;
var
  cfg: TXMLConfig;
  i: Integer;
  s: String;
  b: Boolean;
begin

  WriteLn('Writing a sample XML configuration to "testcfg.xml"...');

  cfg := TXMLConfig.Create(nil);
  cfg.Filename := 'testcfg.xml';
  cfg.SetValue('cfgtest/MainWindow/Constraints/Width', 600);
  cfg.SetValue('cfgtest/MainWindow/Constraints/Height', 400);
  cfg.SetValue('cfgtest/MainWindow/Caption', 'TXMLConfig Test');
  cfg.SetValue('cfgtest/SomeForm/Presets/Preset1/Name', 'Example');
  cfg.SetValue('TipOfTheDay/Disabled', True);
  cfg.Free;

  WriteLn('Ok; now I''ll try to read back all values...');

  cfg := TXMLConfig.Create(nil);
  cfg.Filename := 'testcfg.xml';

  i := cfg.GetValue('cfgtest/MainWindow/Constraints/Width', 0);
  if i <> 600 then
    WriteLn('Invalid value: cfgtest/MainWindow/Constraints/Width, got ', i);

  i := cfg.GetValue('cfgtest/MainWindow/Constraints/Height', 400);
  if i <> 400 then
    WriteLn('Invalid value: cfgtest/MainWindow/Constraints/Height, got ', i);

  s := cfg.GetValue('cfgtest/MainWindow/Caption', '');
  if s <> 'TXMLConfig Test' then
    WriteLn('Invalid value: cfgtest/MainWindow/Caption, got "', s, '"');

  s := cfg.GetValue('cfgtest/SomeForm/Presets/Preset1/Name', '');
  if s <> 'Example' then
    WriteLn('Invalid value: cfgtest/SomeForm/Presets/Preset1/Name, got "', s, '"');

  b := cfg.GetValue('TipOfTheDay/Disabled', False);
  if b <> True then
    WriteLn('Invalid value: TipOfTheDay/Disabled, got ', b);
  cfg.Free;

  WriteLn('Done!');
end.
