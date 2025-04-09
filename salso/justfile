install target = ".":
  R CMD INSTALL {{target}}

build:
  roxido build

expand:
  roxido expand --color always | bat

roxygen2:
  Rscript -e "roxygen2::roxygenise()"

api:
  roxido api

delete-release tag:
  git push --delete origin {{tag}}
  git tag -d {{tag}}

date := `date +"%y.%m.%d"`

new-release: check-clean
  -just delete-release v{{date}}
  -just delete-release latest
  git tag v{{date}}
  git tag latest
  git push --tags

check-clean:
  @if [ -n "$(git status --porcelain)" ]; then \
    echo "Uncommitted changes detected! Aborting."; \
    exit 1; \
  fi
