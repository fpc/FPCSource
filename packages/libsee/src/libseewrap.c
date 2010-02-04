#include <see/see.h>


int SEE_help_Global_eval(interp, input, res)
   struct SEE_interpreter *interp;
   struct SEE_input *input;
   struct SEE_value *res;
{
  SEE_try_context_t c;
        
        
  SEE_TRY(interp,c) {
    SEE_Global_eval (interp, input, res);
  }
  if (SEE_CAUGHT(c)) {
   SEE_VALUE_COPY(res, SEE_CAUGHT(c));
   return 1;
  } else
   return 0;
}                                                                                                          

struct SEE_value * SEE_help_CAUGHT (c)
struct SEE_try_context * c;
{
  return (SEE_CAUGHT(*c));
}

void SEE_help_THROW (interp,v)
 struct SEE_interpreter *interp;
 struct SEE_value * v;
{
  SEE_THROW(interp,v);
}    

void SEE_help_RETHROW (interp,c)
struct SEE_interpreter *interp;
struct SEE_try_context * c;
{
  SEE_RETHROW(interp,* c);
}

void SEE_help_DEFAULT_CATCH (interp,c)
 struct SEE_interpreter *interp;
 struct SEE_try_context * c;
{
  SEE_DEFAULT_CATCH(interp,* c);
}

struct SEE_interpreter * new_SEE_interpreter()
{
  struct SEE_interpreter *interp;

  interp = SEE_NEW(NULL, struct SEE_interpreter);
  SEE_interpreter_init(interp);
  return interp;
}

struct SEE_value * new_SEE_value ()
{
  struct SEE_value * p;
  p = SEE_NEW(NULL, struct SEE_value);
  return p;
}

struct SEE_objectclass * new_SEE_objectclass ()
{
  struct SEE_objectclass * p;
  p = SEE_NEW(NULL, struct SEE_objectclass);
  return p;
}

struct SEE_object * new_SEE_object ()
{
  struct SEE_object * p;
  p = SEE_NEW(NULL, struct SEE_object);
  return p;
}

struct SEE_enumclass * new_SEE_enumclass ()
{
  struct SEE_enumclass * p;
  p = SEE_NEW(NULL, struct SEE_enumclass);
  return p;
}

struct SEE_enum * new_SEE_enum ()
{
  struct SEE_enum * p;
  p = SEE_NEW(NULL, struct SEE_enum);
  return p;
}

struct SEE_native * new_SEE_native ()
{
  struct SEE_native * p;
  p = SEE_NEW(NULL, struct SEE_native);
  return p;
}

struct SEE_scope * new_SEE_scope ()
{
  struct SEE_scope * p;
  p = SEE_NEW(NULL, struct SEE_scope);
  return p;
}

struct SEE_inputclass * new_SEE_inputclass ()
{
  struct SEE_inputclass * p;
  p = SEE_NEW(NULL, struct SEE_inputclass);
  return p;
}

struct SEE_input * new_SEE_input ()
{
  struct SEE_input * p;
  p = SEE_NEW(NULL, struct SEE_input);
  return p;
}

struct SEE_traceback * new_SEE_traceback ()
{
  struct SEE_traceback * p;
  p = SEE_NEW(NULL, struct SEE_traceback);
  return p;
}

struct SEE_context * new_SEE_context ()
{
  struct SEE_context * p;
  p = SEE_NEW(NULL, struct SEE_context);
  return p;
}

struct SEE_growable * new_SEE_growable ()
{
  struct SEE_growable * p;
  p = SEE_NEW(NULL, struct SEE_growable);
  return p;
}

struct SEE_module * new_SEE_module ()
{
  struct SEE_module * p;
  p = SEE_NEW(NULL, struct SEE_module);
  return p;
}

struct SEE_string * new_SEE_string ()
{
  struct SEE_string * p;
  p = SEE_NEW(NULL, struct SEE_string);
  return p;
}

struct SEE_stringclass * new_SEE_stringclass ()
{
  struct SEE_stringclass * p;
  p = SEE_NEW(NULL, struct SEE_stringclass);
  return p;
}

struct SEE_system * new_SEE_system ()
{
  struct SEE_system * p;
  p = SEE_NEW(NULL, struct SEE_system);
  return p;
}

struct SEE_throw_location * new_SEE_throw_location ()
{
  struct SEE_throw_location * p;
  p = SEE_NEW(NULL, struct SEE_throw_location);
  return p;
}

struct SEE_try_context * new_SEE_try_context ()
{
  struct SEE_try_context * p;
  p = SEE_NEW(NULL, struct SEE_try_context);
  return p;
}

void free_SEE_struct(p)
void * p;
{
  free(p);
}

