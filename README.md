# cljblox

Interpreter for Blox, which is a dialect of Lox from [Crafting Interpreters](https://craftinginterpreters.com/). I'm crafting the interpreter in Clojure.

~~Lox programs are valid Blox programs~~. Blox syntax is more lenient in some places, there are some extensions, and differences between implementations:

- [x] The scanner reads a lazy stream of source code instead of using look-ahead.
- [x] Blox supports funky numerical notations like `.999` (decimal), `999.` (decimal), `0x0F` (hex integer), and `0666` (octal integer).
- [ ] Blox has implicit semi-colons allowing you to omit them from most places.
- [ ] Functions have an implicit return. The returned value is the result of last evaluated expression in function body.

We are reading the book in our reading circle at Reaktor Tampere.

## Targets

Run tests

    make test

Lint the code

    make lint

# License

I'm using the [MIT license](./LICENSE), which is permissive for modifications and commercial use. This matches [the repository for the book](https://github.com/munificent/craftinginterpreters), which adopts MIT license for source files.

I'm using some of these source files for my implementation. At the moment, I've copied some `.lox` source files for resources in Blox unit tests.
