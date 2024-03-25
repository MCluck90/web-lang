# Notes

## Loops

```rust
// Infinite loop
loop {
    solveTheHaltingProblem();
}

// Loop over a container
loop list {
    operationOnItem(it);
}

// Loop over a range
loop 0..10 {
    it; // 0, 1, 2, 3, ...
}

// Assign a name to the loop element
loop i: 0..10 {
    i == it;
}
```

## Imports

Imports will all have a source and a series of selectors.

The source indicates where to search for the module and the selectors tell us what to extract from that module.

### Selectors

Selectors may be prefixed with a path to allow for namespacing.

```
/path/to/module/{ imports };
```

Selectors can use the name as exported from the module, provide an alias, or load the entire module under a single name.

```
/path/to/module/{
    normalName,
    originalName as aliased,
    * as entireModule
};
```

### Package Imports

These import from modules outside of the current project. This includes the standard library or third-party libraries.

```rust
use @std:io/{ log };
```

### Absolute Imports

Reference modules from the root of the current project.

```rust
use ~/internal_lib/{ someFunction };
```

### Relative Imports

For referencing modules which are nearby in the file system.

```rust
use ./nested/library/{ someFunction };
use ../../other_library/{ someFunction };
```

## Environments

The main idea of this language is to be able to write a single codebase and have it work as both a frontend and a server. What are the implications of this?

Any given chunk of code can be run in 1 of 3 states:

1. Frontend
2. Backend
3. Isomorphic/Both

What happens when trying to interact with code from one environment in another? The left side indicates the execution environment and the top indicates which environment the function was written for.

|          | Frontend             | Backend              | Isomorphic           |
| -------- | -------------------- | -------------------- | -------------------- |
| Frontend | Normal function call | Remote call          | Normal function call |
| Backend  | Remote call          | Normal function call | Normal function call |

When code is not given a specific environment, it will run in both.

How does calling across environments change the compiled code? Functions become colored. For now, everything that does a cross environment call will become asynchronous and do automatic awaiting.

```rust
back {
    fn logBackend() {
        log("I run on the backend");
    }

    logFrontend();
    logBackend();
}

front {
    fn logFrontend() {
        log("I run on the frontend");
    }

    logBackend();
    logFrontend();
}

logFrontend();
logBackend();
```

should compile to something functionally equivalent to this:

```js
/**
 * frontend.js
 */
function logFrontend() {
  console.log('I run on the frontend')
}

function logBackend() {
  return fetch('/logBackend')
}

server.on('message', (msg) => {
  if (msg.fnId === 'logFrontend') {
    logFrontend()
  }
})

await logBackend()
logFrontend()

// from the isomorphic section
logFrontend()
await logBackend()

/**
 * backend.js
 */
function logFrontend() {
  return client.send('message', { fnId: 'logFrontend' })
}

function logBackend() {
  console.log('I run on the backend')
}

app.get('/logBackend', () => {
  logBackend()
})

await logFrontend()
logBackend()

// from the isomorphic section
await logFrontend()
logBackend()
```

Next case: defining a function which calls in to another environment without defining one

```rust
fn log() { // Becomes backend by association
    logBackend();
}
log();
```

compiles to:

```js
/**
 * frontend.js
 */
async function log() {
  await logBackend()
}

function logBackend() {
  return fetch('/logBackend')
}

await log()

/**
 * backend.js
 */
function log() {
  logBackend()
}

function logBackend() {
  console.log('I run on the backend')
}

log()
```

This sort of coloring will climb all the way back up to the root. As soon as you have one asynchronous/cross-environment call, the whole call stack needs to be asynchronous.

What about functions that do both?

```rust
fn log() {
    logFrontend();
    logBackend();
}

log();
```

becomes

```js
/**
 * frontend.js
 */
async function log() {
  logFrontend()
  await logBackend()
}

await log()

/**
 * backend.js
 */
async function log() {
  await logFrontend()
  logBackend()
}

await log()
```
