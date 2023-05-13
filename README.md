generic
=======

A port of Vesa Karvonen's generic library to smlpkg, targeting MLton.

It is an implementation of type indexed values
so that you can derive pretty, hash, ord, arbitrary, pickle, uniplate, etc from one representation.

To add the library with smlpkg, run:

```
smlpkg add github.com/DarinM223/generic
```

Then include:

```
lib/github.com/DarinM223/generic/lib-all.mlb
```

into your mlb file to derive everything. If you only want to derive certain things, include:

```
lib/github.com/DarinM223/generic/lib.mlb
```

and

```
lib/github.com/DarinM223/generic/with/generic.sml
lib/github.com/DarinM223/generic/with/eq.sml
...
```

for the things you want to derive.