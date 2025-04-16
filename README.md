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

Now, install Cargo (the Rust package manager) as described in
[INSTALL](salso/INSTALL).

Then, install the `remotes` packages:

```r
install.packages("remotes")
```

Now, Install the package itself:

```
remotes::install_github("dbdahl/salso/salso")
```

