{
    Copyright (c) 2017 Karoly Balogh

    Simple Alert Form
    Example program for Free Pascal's Atari TOS bindings

    This example program is in the Public Domain under the terms of
    Unlicense: http://unlicense.org/

 **********************************************************************}

{$APPTYPE GUI}
program higem;

uses
  aes;

begin
  appl_init;

  form_alert(1,'[4][Hi Atari GEM World!|'+
               '|This is Free Pascal here.][Cool]');

  appl_exit;
end.
