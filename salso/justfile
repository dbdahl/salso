install target = ".":
  R CMD INSTALL {{target}}

build:
  roxido build

expand:
  roxido expand --color always | bat

roxygen2:
  Rscript -e "roxygen2::roxygenise()"

api:
  cd src/rust/roxido; cargo doc --open

delete-release tag:
  git push --delete origin {{tag}}
  git tag -d {{tag}}

new-release:
  -just delete-release `date +"v%y.%m.%d"`
  -just delete-release latest
  git tag `date +"v%y.%m.%d"`
  git tag latest
  git push --tags
