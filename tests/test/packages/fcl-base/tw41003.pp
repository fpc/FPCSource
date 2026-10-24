{ Resourcestrings keep their value in units finalized after gettext }
program tw41003;

{$mode objfpc}{$h+}

uses
  uw41003, gettext;

begin
  if rsHello<>'Hello' then
    halt(2);
end.
