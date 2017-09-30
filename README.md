# haskell_soe

## Set up

```
apt install haskell-platform
cabal update
cabal install HGL doctest
echo "export PATH="$HOME/.cabal/bin:$PATH" >> ~/.bashrc
```

To see if it worked:

```
./compile chapters/chapter3_main.hs
./chapters/chapter3_main
doctest chapters/chapter7.hs
```
