## Installation

### Installation from CRAN

The latest release is available on [CRAN](https://cran.r-project.org/):

```r
install.packages('salso')
```

### Installation from Source

You can also install the latest development version from source.

First install the development tools. On MacOS, install Xcode command line tools
by running `sudo xcode-select --install`. On Windows, install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/).

Then install the `remotes` and `cargo` packages:

```r
install.packages("remotes")
remotes::install_github("dbdahl/cargo-framework/cargo")
```

Now install the Rust toolchain:

```
cargo::install(force = TRUE)
```

Finally, install the package itself:

```
remotes::install_github("dbdahl/salso/salso")
```

