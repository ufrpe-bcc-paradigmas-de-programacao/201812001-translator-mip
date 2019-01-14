# Translator Mip

## Setup Project

1. Clone this repository doing:

```
git clone https://github.com/ufrpe-bcc-paradigmas-de-programacao/201812001-translator-mip.git
```

2. Enter the cloned folder

3. Install the [Dependencies](#dependencies)

## How to use

1. Run the program
    * With npm (inside root folder): `npm start`
    * With GHCi (inside src folder): `ghci "./translator.hs"`

2. Type `convert "file-path"`

3. It's all done! You can check the output `lrg.mif` in `output` folder

## Dependencies:

- [Haskell Compiler (GHC)](https://www.haskell.org/downloads)
- Install [Cabal](https://www.haskell.org/cabal/download.html)

 Now Choose your path

 - [Install Using NPM](#npminstall)
 - [Install Using Cabal](#cabalinstall)

<h4 id="npminstall"> Using NPM </h4>

On your terminal run:
1. `npm run installDeps`

<h4 id="cabalinstall"> Using Cabal </h4>

On your terminal run:
1. `cabal update`
1. `cabal install hex text split`

## Credits

- [DwarfThief](https://github.com/DwarfThief)
- [brunohgv](https://github.com/brunohgv)
- [otavioalves2](https://github.com/otavioalves2)