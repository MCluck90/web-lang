# Notes

## Generating blocks with return statements
I want to allow for early returns. However, to enable blocks as expressions, the code is generated as anonymous
functions. For example:

```ts
let checkErrors() {
    let errorLevel = if numOfErrors > 10 {
        print('This is really bad');
        "Critical"
    } else {
        print('This is pretty bad')
        "Danger"
    }
    // ...
}
```

compiles to

```js
const checkErrors = () => {
    const errorLevel = numOfErrors > 10
        ? (() => {
            print(`This is really bad`);
            return `Critical`;
        })()
        : (() => {
            print(`This is pretty bad`);
            return `Danger`;
        })();
    // ...
}
```

which means that adding our own return statements inside of `if` expressions would not work.

```ts
let checkErrors() {
    let errorLevel = if numOfErrors > 10 {
        if ignore {
            return "None"
        }

        print('This is really bad');
        "Critical"
    } else {
        if ignore {
            return "None"
        }

        print('This is pretty bad')
        "Danger"
    }
    // ...
}
```

would compile to

```js
const checkErrors = () => {
    const errorLevel = numOfErrors > 10
        ? (() => {
            if (ignore) {
                return "None"
            }
            print(`This is really bad`);
            return `Critical`;
        })()
        : (() => {
            if (ignore) {
                return "None"
            }
            print(`This is pretty bad`);
            return `Danger`;
        })();
    // ...
}
```

So `return` statements have no way of escaping the correct scope.

Instead of compiling all blocks down to immediately executed function expressions, I can instead introduce temporary
variables that they would assign to and use those temporary values in place of the expression.

```js
const checkErrors = () => {
    let temp;
    if (numOfErrors > 10) {
        if (ignore) {
            return `None`;
        }
        print(`This is really bad`);
        temp = `Critical`;
    } else {
        if (ignore) {
            return `None`;
        }
        print(`This is pretty bad`);
        temp = `Danger`
    }
    const errorLevel = temp;
    // ...
}
```

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
    i; // 0, 1, 2, 3, ...
}

// Loop with initializer, condition, and post-loop expression
loop (let i = 0; i < 10; i++) {
    i; // 0, 1, 2, 3, ...
}
```
