# evm-asm
This is an evm assembler written in a dialect of `Lisp` called `Scheme (Racket)`.
I have chosen this language for it's `s-exp` parsing and **MASSIVE** pattern matching.

The original code comes from [Harsh Vakharia](https://github.com/harshjv/).

The `manOP` code took me about 30 minutes to optimize (after spending a while reading the yellow paper and asking people around).

The syntax is LLL-like, yet not really at the same time.
You can either call this the `evm assebmler` or just `Ting` (my name).
> I have always wanted to name a program with my name, it just feels funny.

Porting the assembly output of `lllc` to `Ting` is a bijective mapping.
> See `example.rkt` and `ETHSwap.lll.asm`

# TO-DO
* Rename `evm-assemble` to something more sensible.
* Make it actually return something other than `(void)`.
* Add benchmark for expire.

# Install
Install the latest version of `drracket` from your package manager or [here (this text here is to make the link longer)](https://download.racket-lang.org/).

# Run
To run the example file, do
```bash
racket example.rkt
```

# How good is it?
* Solidity (reference):
    * Bytesize: 796
    * Gas cost used to deploy: 301615
    * Gas cost used to call `claim`: 32145
* LLL:
    * Bytesize: 226 (3.52x reduction)
    * Gas cost used to deploy: 182311 (119304 less; 1.65x reduction)
    * Gas cost used to call `claim`: 32152 (7 more)
* My thingy:
    * Bytesize: 155 (5.14x reduction)
    * Gas cost used to deploy: 166222 (135393 less; 1.81x reduction)
    * Gas cost used to call `claim`: 23384 (8761 less)
