# For the difference between 'all' and 'ci': 'all' is the default run with no arguments. So, as the
# default we will format the code, but not run a "check" of it, which is used in CI to fail the
# running job if code wasn't formatted.
all: setup build-cabal build-stack format
ci: build-cabal build-stack format-check
setup: setup-ghc setup-cabal setup-stack setup-extra-tools

# Note that the ghc-versions for cabal and the stack yamls match which version of ghc they end up
# using. Also note that by use ?= we allow this to be defined with environment variables so CI can
# run a subset of them on different platforms. This is particularly helpful when something is
# unreleased and/or broken upstream. Like not having a proper release for aarch64-darwin or
# something.
GHC_VERSIONS ?= 9.4.5 9.2.8
STACK_YAMLS ?= stack.yaml stack-lts-20.19.yaml

.PHONY: setup-ghc
setup-ghc:
# This will prefetch/build the required versions of ghc upfront
.for GHC_VERSION in ${GHC_VERSIONS}
	ghcup install ghc ${GHC_VERSION}
.endfor

.PHONY: setup-cabal
setup-cabal:
	cabal update
# This will build all the dependencies for the various ghc versions
.for GHC_VERSION in ${GHC_VERSIONS}
	cabal build --only-dependencies -w ghc-${GHC_VERSION} --flag ci
.endfor

.PHONY: setup-stack
setup-stack:
	stack build --only-dependencies --flag henforcer:ci

.PHONY: setup-extra-tools
setup-extra-tools:
# Note: We don't actually need n versions of formatting/linting tools though, so only get ones with
# the default stack.yaml
	stack install fourmolu

.PHONY: build-cabal
build-cabal:
.for GHC_VERSION in ${GHC_VERSIONS}
	cabal build -w ghc-${GHC_VERSION} --flag ci
.endfor

.PHONY: build-stack
build-stack:
.for STACK_YAML in ${STACK_YAMLS}
	stack --stack-yaml ${STACK_YAML} build --flag henforcer:ci
.endfor

.PHONY: format
format:
	fourmolu -i app plugin src

.PHONY: format-check
format-check:
	fourmolu -m check app plugin src
