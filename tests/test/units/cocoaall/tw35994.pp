
{$MODE OBJFPC}
{$MODESWITCH OBJECTIVEC1}

program test;

uses
  CocoaAll;

var
  obj: NSObject;
  path: NSString;
  dict: NSDictionary;
  mDict: NSMutableDictionary;
  pool: NSAutoReleasePool;
begin
  pool := NSAutoReleasePool.alloc.init;
  obj := NSObject.alloc.init;

  path := NSSTR('');
  dict := NSDictionary.dictionaryWithContentsOfFile(path);
  dict := NSDictionary.alloc.initWithContentsOfFile(path); // ERROR: got "NSArray" expected "NSDictionary"
  dict := NSDictionary(NSDictionary.alloc).initWithContentsOfFile(path);

  dict := NSMutableDictionary.dictionaryWithContentsOfFile(path);
  mDict := NSMutableDictionary.dictionaryWithContentsOfFile(path); // ERROR: got "NSDictionary" expected "NSMutableDictionary"
  dict := NSMutableDictionary.alloc.initWithContentsOfFile(path); // ERROR: got "NSArray" expected "NSDictionary"
  mDict := NSMutableDictionary.alloc.initWithContentsOfFile(path); // ERROR: got "NSArray" expected "NSDictionary"

  pool.release;
end.
