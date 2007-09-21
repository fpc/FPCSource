{
    This file is part of the Free Component Library

    JSON Config file demo
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program confdemo;

{$mode objfpc}{$H+}

uses
  Classes,
  { add your units here }
  jsonconf;

Procedure TestConf;

Var
  C : TJSONConfig;
  L : TStrings;
  I : Integer;
  
begin
  // TJSONConf is component, so needs an owner.
  C:=TJSONConfig.Create(nil);
  Try
    // Set filename. This will read the file.
    C.FileName:='sample.conf';
    // Set an integer value "a" equal to 1 in the root object
    C.SetValue('/a',1);
    // Set a integer value "a" equal to 2 in the object "b" below root.
    C.SetValue('b/a',2);
    // Set a string value "b" equal to 1 in the object "b" below root.
    C.SetValue('b/b','Some String');
    // Set a float value "c" equal to 1.23 in the object "b" below root.
    C.SetValue('b/c',1.23);
    // Set a boolean value "d" equal to "False" in the object "b" below root.
    C.SetValue('b/d',False);
    // Read values:
    // Integer. If none found, 0 is returned)
    Writeln('/a :',C.GetValue('/a',0));
    // String. If none found, a default 'XYZ' is returned)
    Writeln('/b/b :',C.GetValue('/b/b','XYZ'));
    // Float. If none found, 0 is returned)
    Writeln('/b/c :',C.GetValue('/b/c',0));
    // Boolean. If none found, true is returned)
    Writeln('/b/d :',C.GetValue('/b/d',true));
    // You can open a key. All paths are then relative to the open key.
    // The default open key is the root key.
    // The second element determines if the key should b created if it does not exist.
    C.OpenKey('/b',False);
    // Read relative to b
    Writeln('a, relative to key (/b):',C.GetValue('a',0));
    // Absolute paths disregard the open key
    Writeln('/a, absolute:',C.GetValue('/a',0));
    // Reset or closekey reset the open key to the root key.
    C.OpenKey('/b/c/d/e',True);
    C.SetValue('q','Q is good for you');
    // Opening keys also works relative:
    C.OpenKey('/b',False);
    Writeln('a, in b : ',C.GetValue('a',0));
    C.OpenKey('c/d/e',False);
    Writeln('q, in /b, then c/d/e : ',C.GetValue('q',''));
    C.ResetKey;
    C.OpenKey('/b2',True);
    C.OpenKey('/b3',True);
    L:=TStringList.Create;
    try
      // You can enumerate keys below a certain key:
      C.EnumSubKeys('/',L);
      Writeln('Found ',L.Count,' keys below root key: ');
      For I:=0 to L.Count-1 do
        Writeln(i+1,': ',L[I]);
      // You can also enumerate the values below a certain key:
      L.Clear;
      C.EnumValues('/b',L);
      Writeln('Found ',L.Count,' values below "/b" key: ');
      For I:=0 to L.Count-1 do
        Writeln(i+1,': ',L[I]);
    finally
      L.Free;
    end;
    // Write all in-memory changes to disk
    C.Flush;
  Finally
    C.Free;
  end;
end;

begin
  TestConf;
end.

