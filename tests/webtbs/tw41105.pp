program Project1;

// id
// {$WARN 5023 ON}    // compiles
// {$WARN 5023 OFF}   // compiles
{$WARN 5023 +}     // ERROR
{$WARN 5023 -}     // ERROR
{$WARN 5023+}      // ERROR
{$WARN 5023-}      // ERROR
{$WARN 5023 ERROR} // compiles

// name
{$WARN NO_RETVAL ON}    // compiles
{$WARN NO_RETVAL OFF}   // compiles
{$WARN NO_RETVAL +}     // ERROR
{$WARN NO_RETVAL -}     // ERROR
{$WARN NO_RETVAL+}      // ERROR
{$WARN NO_RETVAL-}      // ERROR
{$WARN NO_RETVAL ERROR} // compiles

begin
end.
