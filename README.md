### Binary installation

On most platforms, you can install the package using:

```r
options(repos=c(dbdahl='https://dbdahl.r-universe.dev', CRAN='https://cloud.r-project.org'))
install.packages('salso')
````

### Source installation

If binary installation fails, you can install from source. First install the development tools.
On MacOS, install Xcode command line tools by running `sudo xcode-select --install`.
On Windows, install [Rtools42](https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html). 

Then install the `remotes` and `cargo` packages and then install Cargo (the Rust package manager):

```r
install.packages("remotes")
remotes::install_github("dbdahl/cargo-framework/cargo")
cargo::install(force=TRUE)
```

Finally, install the package itself:

````
remotes::install_github("dbdahl/salso/salso")
```

