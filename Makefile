# For the difference between 'all' and 'ci': 'all' is the default run with no arguments. So, as the
# default we will format the code, but not run a "check" of it, which is used in CI to fail the
# running job if code wasn't formatted.
all: setup format outdated build-cabal build-stack test-cabal
ci: outdated build-cabal build-stack test-cabal format-check hlint
setup: setup-cabal setup-stack setup-extra-tools

# Note that the ghc-versions for cabal and the stack yamls match which version of ghc they end up
# using. Also note that by use ?= we allow this to be defined with environment variables so CI can
# run a subset of them on different platforms. This is particularly helpful when something is
# unreleased and/or broken upstream. Like not having a proper release for aarch64-darwin or
# something.
GHC_VERSIONS ?= 9.4.8 9.6.7 9.8.4 9.10.3 9.12.2
STACK_YAMLS ?= stack.yaml

.PHONY: setup-ghc
setup-ghc:
# This will prefetch/build the required versions of ghc upfront
.for GHC_VERSION in ${GHC_VERSIONS}
	command -v ghc-${GHC_VERSION} || ghcup install ghc ${GHC_VERSION}
.endfor

.PHONY: setup-cabal
setup-cabal: setup-ghc
	cabal update
# This will build all the dependencies for the various ghc versions
.for GHC_VERSION in ${GHC_VERSIONS}
	cabal build -j --only-dependencies -w ghc-${GHC_VERSION} --flag ci
.endfor

.PHONY: setup-stack
setup-stack:
	stack build --only-dependencies --flag henforcer:ci

.PHONY: setup-extra-tools
setup-extra-tools: setup-fourmolu setup-hlint setup-weeder

.PHONY: setup-fourmolu
setup-fourmolu:
	 stack install fourmolu

.PHONY: setup-hlint
setup-hlint:
	command -v hlint || stack install hlint

.PHONY: setup-weeder
setup-weeder:
	command -v weeder || stack install weeder

.PHONY: build-cabal
build-cabal:
.for GHC_VERSION in ${GHC_VERSIONS}
	cabal build -j -w ghc-${GHC_VERSION} --flag ci
.endfor

.PHONY: build-stack
build-stack:
.for STACK_YAML in ${STACK_YAMLS}
	stack --stack-yaml ${STACK_YAML} build --flag henforcer:ci
.endfor

.PHONY: test-cabal
test-cabal:
.for GHC_VERSION in ${GHC_VERSIONS}
	cabal test -j -w ghc-${GHC_VERSION} --flag ci
.endfor

.PHONY: test-stack
test-stack:
.for STACK_YAML in ${STACK_YAMLS}
	stack --stack-yaml ${STACK_YAML} test --flag henforcer:ci
.endfor

.PHONY: outdated
outdated:
	cabal outdated --exit-code

.PHONY: format
format: setup-extra-tools
	~/.local/bin/fourmolu -i plugin src test

.PHONY: format-check
format-check:
	~/.local/bin/fourmolu -m check plugin src test

.PHONY: weeder
weeder: setup-weeder test-stack
	stack exec weeder -- --require-hs-files --hie-directory .stack-work/dist/

.PHONY: hlint
hlint: setup-extra-tools
	stack exec hlint -- plugin src
