{%NORUN}
{$mode objfpc}

{
  Test RTTI directive
}
program texrtti1;

{$RTTI EXPLICIT PROPERTIES([])}
{$RTTI EXPLICIT FIELDS([])}
{$RTTI EXPLICIT METHODS([])}

{$RTTI EXPLICIT PROPERTIES([vcPrivate,vcProtected,vcPublic,vcPublished])}
{$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
{$RTTI EXPLICIT METHODS([vcPrivate,vcProtected,vcPublic,vcPublished])}

{$RTTI EXPLICIT PROPERTIES([vcPrivate,vcProtected,vcPublic,vcPublished])
                FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])
                METHODS([vcPrivate,vcProtected,vcPublic,vcPublished])}

begin
end.
