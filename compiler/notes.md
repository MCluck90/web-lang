# Notes

## Loops

```rust
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

// Custom loop condition
mut i = 0;
loop (i < 10) {
    i += 1;
}

// Loop with initializer and condition
loop (let i = 0; i < 10) {
    i += 1; // 0, 1, 2, 3, ...
}

// Loop with initializer, condition, and post-loop expression
loop (let i = 0; i < 10; i++) {
    i; // 0, 1, 2, 3, ...
}
```

### Problem

Harder to talk about in real life. It's easier to say "you should use a for loop" than it is to say "you should use a
loop with an initializer, condition, and post-loop expression."

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
