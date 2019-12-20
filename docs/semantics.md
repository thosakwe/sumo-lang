## ABI
While Sumo programs *technically* follow the C calling 
convention, in reality, every function requires additional
information to be able to support constructs like
`try`/`catch`/`finally`, as well as references like `this`.

Every Sumo function's first parameter is a pointer to
an array (more specifically, a hash table) of types,
mapped to function pointers.

When a `throw` occurs,
the thrown value's type will be looked up in this table,
and the found function will be invoked. If none is found,
then the program will abort.

When a `try` is set up, a new hash table of error handlers
is set up, including those from the parent, and pointers
to any local `catch` blocks.

## Debug mode
Enable debug mode by passing the `-g` flag to the `sumoc`
command. Note that if you change the debug status of your
program, you should recompile *all files*, as the
semantics may change in an incompatible way.

In debug mode, at the *entry point* of a Sumo program, a
setup routine creates a minimal error handler table that 
catches any exception, and, using the emitted symbol
information, reports the error type, and stack trace,
among other things.

In addition, in debug mode, a global call stack is modified
before every action, so when an error occurs, its location
information can be reported.