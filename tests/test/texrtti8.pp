{%NORUN}
{$mode objfpc}

{
  Test RTTI directive (inherit clause)
}
program texrtti8;
uses
  typinfo;


{$RTTI INHERIT PROPERTIES([])}
{$RTTI INHERIT FIELDS([])}
{$RTTI INHERIT METHODS([])}

{$RTTI INHERIT PROPERTIES([vcPrivate,vcProtected,vcPublic,vcPublished])}
{$RTTI INHERIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
{$RTTI INHERIT METHODS([vcPrivate,vcProtected,vcPublic,vcPublished])}

{$RTTI INHERIT PROPERTIES([vcPrivate,vcProtected,vcPublic,vcPublished])
               FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])
               METHODS([vcPrivate,vcProtected,vcPublic,vcPublished])}

begin
end.
