install target = ".":
  R CMD INSTALL {{target}}

build:
  roxido build

expand:
  Rscript -e "cmd <- 'roxido expand --color always'; if (nzchar(Sys.which('bat'))) cmd <- paste(cmd, '| bat'); system(cmd)"

roxygen2:
  Rscript -e "roxygen2::roxygenise()"

api:
  roxido api

delete-release tag:
  git push --delete origin {{tag}}
  git tag -d {{tag}}

date := datetime("%y.%m.%d.%H.%M")

new-release: check-clean
  -just delete-release v{{date}}
  -just delete-release latest
  git tag v{{date}}
  git tag latest
  git push
  git push --tags

check-clean:
  git diff --quiet --exit-code
  git diff --quiet --exit-code --cached
