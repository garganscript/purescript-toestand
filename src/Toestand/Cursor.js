"use strict";

// We take the dictionaries because we can do the preparation in
// purescript space and keep the type checking.
exports._readDict       = (dict) => () => dict;
exports._writeDict      = (dict) => () => dict;
exports._monadDelayDict = (dict) => () => dict;

// Then we bundle up the purescript functions we've prepared into an opaque type so we can erase it.
exports._cursor = (read) => (listen) => (write) => { read, listen, write };
exports._view = (read) => (listen) => { read, listen };

// Now we expose some simple wrappers for the three functions

// read works because MonadDelay is always an effect at Runtime
exports._read   = (delay) => ()     => (cell) => cell.read(delay);
exports._listen = (cell)  => (fn)   => cell.listen(fn)();
exports._write  = (value) => (cell) => cell.write(value);
