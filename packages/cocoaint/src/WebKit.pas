unit WebKit;

{$mode objfpc}
{$modeswitch objectivec1}

interface

{$linkframework WebKit}

uses
  ctypes, CocoaAll, MacOSAll;
  
{$define INTERFACE}

{$include webkit/UndefinedTypes.inc}

{$define HEADER}
{$include webkit/WebKit.inc}
{$undef HEADER}

{$define TYPES}
{$include webkit/WebKit.inc}
{$undef TYPES}

{$define RECORDS}
{$include webkit/WebKit.inc}
{$undef RECORDS}

type
{$define FORWARD}
{$include webkit/WebKit.inc}
{$undef FORWARD}

{$include webkit/UndefinedClasses.inc}

{$define CLASSES}
{$include webkit/WebKit.inc}
{$undef CLASSES}
 
{$define PROTOCOLS}
{$include webkit/WebKit.inc}
{$undef PROTOCOLS}

{$define FUNCTIONS}
{$include webkit/WebKit.inc}
{$undef FUNCTIONS}

{$define EXTERNAL_SYMBOLS}
{$include webkit/WebKit.inc}
{$undef EXTERNAL_SYMBOLS}

{$define USER_PATCHES}
{$include webkit/WebKit.inc}
{$undef USER_PATCHES}

{$undef INTERFACE}
implementation
{$define IMPLEMENTATION}

{$define USER_PATCHES}
{$include webkit/WebKit.inc}
{$undef USER_PATCHES}

{$undef IMPLEMENTATION}
end.