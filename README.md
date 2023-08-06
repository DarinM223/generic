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

For building with Poly/ML, you would want a build script similar to [this](https://raw.githubusercontent.com/DarinM223/smlgen/master/build_polyml.sh) that generates a `build.sml` PolyML file from a `.mlb` file. Then replace all of the use statements to `extended-basis` files with:

```sml
OS.FileSys.chDir "lib/github.com/DarinM223/extended-basis";
use "basis.sml";
OS.FileSys.chDir "../../../../";
```

Then replace the
```sml
use "lib/github.com/DarinM223/random/detail/ml/mlton/random-dev.sml";
```
line with:
```sml
use "lib/github.com/DarinM223/random/detail/ml/common/random-dev.sml";
```

Finally replace the use statments to `generic` with:

```sml
OS.FileSys.chDir "lib/github.com/DarinM223/generic";
use "lib-all.sml";
OS.FileSys.chDir "../../../../";
```