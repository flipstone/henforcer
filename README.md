# henforcer

`henforcer` is a `Haskell` enforcer of checks to help keep your forest of Haskell modules organized
cleanly.

## Installation

`henforcer` has not yet been released to hackage, so you should install it from source either by
cloning and building the repo, or adding a extra dep to your `stack.yaml` like below and running
`stack install henforcer`

```
extra-deps:
- git: https://github.com/flipstone/henforcer
  commit: <SHA of the latest master commit>
```

## Initialization

Once the `henforcer` command is installed you'll need to initialize henforcer for your project. `cd`
to your product directory and run the following command:

`henforcer --init`

or

`stack exec henforcer --init`

This will create an empty henforcer configuration file at `henforcer.dhall` in the root of your
project plus a `.henforcer/package.dhall` file that defines the configuration file format and is
imported by `henforcer.dhall`.

_Note: `henforcer` may require a later version of `dhall` than is available in earlier lts stackage
snapshots. You may need to add an extra-dep like below to get a compatible version of `dhall`, and
its dependency `atomic-write`. You may need to tweak `dhall` version below based on the `repline`
version in your lts as well -- use a `dhall` > `1.32` if your LTS has `repline` >= `0.4`.

```
# henforcer needs a later dhall, which needs a later atomic-write
- dhall-1.32.0
- atomic-write-0.2.0.7
```


## Execution

`henforcer` is a GHC plugin, thus it is executed during compliation. To enable the plugin during
compliation with `cabal` or `stack`, add `henforcer` as a dependency in your cabal file or
package.yaml as applicable and add `-fplugin Henforcer` to `ghc-flags`. Specifying plugin options is
done with `ghc-flags` as well. Currently only supported is the path to the configuration, which if
it is at `foo/bar/henforcer.dhall` this would be as `-fplugin-opt=Henforcer:-c
foo/bar/henforcer.dhall`.

## Configuration

`henforcer` uses `dhall` files for configuration. The `.henforcer/package.dhall` file created by
`henforcer --init` defines the configuration options that are available as well as default values
for them all. You can use it as a handy reference for configuring `henforcer`.

Note that the default `henforcer` configuration does not enforce any particular rules, so running
`henforcer` immediately after installation will not report module structure errors.

Source options are described below. More precise definitions can be found in the
[package.dhall](data/package.dhall) file.

### Module Trees

`henforcer` uses "module trees" as part of its configuration. A "module tree" refers to a root
module (e.g. `Data.Text`) and all the modules are prefixed by it (e.g. `Data.Text.Encoding` and
`Data.Text.Lazy`). For modules in your project this almost always means a `src/Foo/MyModule.hs` file
and any `.hs` files contained inside the `src/Foo/MyModule` directory.

### Tree Dependencies

The `treeDependencies` options lets you declare that one module tree depends on other
trees. Declaring such a dependency tells `henforcer` that you don't want the dependency targetso to
import anything from the dependent tree, which would cause a backwards dependency rendering the two
module trees logically inseparable.  `henforcer` will report any imports causing a backward
dependency as an error.

### Encapsulated Trees

The `encapsulatedTrees` options lets you declare that the root of a module tree is effectively a
public interface that any modules outside the tree should be using. `henforcer` will report an error
if any module outside the tree attempts to import a module from inside the encapsulated tree.

### Qualification Rules

The `allowedQualifications` option lets you declare that certain modules must or must not be
imported as `qualified`, if the qualification must be prepositive or postpositive, what aliases may
be used for the module, and if the import should be marked with 'safe'.

Several helpers exist to make writing these rules easier:
- `unqualified`
  Specifies an import must be unqualified, without an alias, such
  as `import UnliftIO`.

- `qualified`
  Specifies an import must be qualified, in the prepositive, without an alias, such
  as `import qualified UnliftIO`.

- `qualifiedPost`
  Specifies an import must be qualified, in the postpositive, without an alias, such
  as `import UnliftIO qualified`.

- `unqualifiedAs`
  Specifies an import must be unqualified, with an alias, such
  as `import Data.Text as T`.

- `qualifiedAs`
  Specifies an import must be qualified, in the prepositive, with an alias, such
  as `import qualified Data.Text as T`.

- `qualifiedPostAs`
  Specifies an import must be qualified, in the postpositive, with an alias, such
  as `import Data.Text qualified as T`.

- `qualifiedEither`
  Specifies an import must be qualified, in either the prepositive or postpositive, without an
  alias, such as `import qualified UnliftIO` or `import UnliftIO qualified`, producing a list of
  allowed qualifications.

- `qualifiedEitherAs`
  Specifies an import must be qualified, in either the prepositive or postpositive, with an alias,
  such as `import qualified Data.Text as T` or `import Data.Text qualified as T`, producing a list of
  allowed qualifications.

- `setWithSafe`
  Mark an import as requiring to be imported 'safe', this is meant to be chained with one of the
  other helpers.

- `onlySafe`
  Mark a list of imports as required to be imported 'safe'.

### Open Imports

`henforcer` allows for checks around open imports. A maximum number of completely open imports can
be specified. This does not include imports that have an associated alias, or those that are
'hiding'.

The checks for this can be specified as a default to apply to all modules with the option
`defaultAllowedOpenUnaliasedImports` and on a module by module basis with the option
`perModuleOpenUnaliasedImports`. Any per module check specified will have precedence over a default.

The following helper functions exist for these checks:

- `defaultMaxAllowedOpenUnaliasedImports`
  Allows to easily set the default maximum for the open import check.
