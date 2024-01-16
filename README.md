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

_Note: `henforcer` requires a handful of packages that are not on stackage as of lts-22.7. The following is likely to needed in the `extra-deps` section of your `stack.yaml`

```
# henforcer needs pollock and tomland
- pollock-0.1.0.0
- tomland-1.3.3.2
# tomland needs validation-selective
```

## Initialization

Once the `henforcer` command is installed you'll need to initialize henforcer for your project. `cd`
to your product directory and run the following command:

`henforcer --init`

or

`stack exec henforcer --init`

This will create a default `henforcer` configuration file at `henforcer.toml` in the root of your
project.


## Execution

`henforcer` is a GHC plugin, thus it is executed during compliation. To enable the plugin during
compliation with `cabal` or `stack`, add `henforcer` as a dependency in your cabal file or
package.yaml as applicable and add `-fplugin Henforcer` to `ghc-flags`. Specifying plugin options is
done with `ghc-flags` as well. Currently only supported is the path to the configuration, which if
it is at `foo/bar/henforcer.toml` this would be as
`-fplugin-opt=Henforcer:-cfoo/bar/henforcer.toml`.

## Configuration

`henforcer` uses `TOML` files for configuration. Note that the default `henforcer` configuration
does not enforce any particular rules, so running `henforcer` immediately after installation and
initialization will not report errors.

Configuration concepts and options are described below. Also the `examples/henforcer.toml` file
shows a variety of usage possibilities.

### Concepts

#### Module Trees

`henforcer` uses "module trees" as part of its configuration. A "module tree" refers to a root
module (e.g. `Data.Text`) and all the modules are prefixed by it (e.g. `Data.Text.Encoding` and
`Data.Text.Lazy`). For modules in your project this almost always means a `src/Foo/MyModule.hs` file
and any `.hs` files contained inside the `src/Foo/MyModule` directory.
### Configuration reference

#### forAnyModule

Required: Yes

The `forAnyModule` key is a TOML table containing all of the checks that apply when compiling any
given module. You can think of these as "global" but they do *not* apply to multiple modules at
once.

##### allowedAliasUniqueness

Required: No

`allowedAliasUniqueness` allows to check either that all aliases in the module being compiled are
unique except for some, or that a given set of aliases is unique but others may be duplicated. This
is specified as a TOML table with two keys:

- `aliases` is an array of strings that are the aliases to be checked with.
- `allAliasesUnique` is a boolean field. When true it determines that every alias should be unique,
  except those given. When false it determines that only the given aliases should correspond to
  exactly one import.

The following determines that aliases should be unique by default but that repeated use of "M" is
allowed.

```toml
[forAnyModule]
allowedAliasUniqueness = { allAliasesUnique = true, aliases = [ "M" ] }
```

##### allowedOpenUnaliasedImports

Required: No

`allowedOpenUnaliasedImports` is a non-negative integer that specifies how many imports are allowed
to be done without using the `qualified` keyword, as well as without using an `alias`. For example
`import Prelude`. This allows for a check similar to `-Wno-missing-import-lists` but that is
explictly without issue for modules that re-export others using an alias.

##### allowedQualifications

Required: No

`allowedQualifications` is an array of tables that represent how certain modules should be
imported. This can be thought of as a map of module name to a list of ways that module may be
imported.

###### module

Required: Yes

`module` is a string of the module name.

###### importScheme

Required: Yes

`importScheme` defines the way that the module may be imported. The value is an array of
tables. Which allows for several separate schemes to be specified for a module. The table includes
checks for qualification, aliasing, safe imports and package qualification. Each of those are
detailed below.

```toml

[[forAnyModule.allowedQualifications]]
module = "UnliftIO"
[[forAnyModule.allowedQualifications.importScheme]]
qualified = { qualifiedPre = true, qualifiedPost = true }
alias = "Foo"
packageQualified = "unliftio"
```

####### qualified

Required: Yes

`qualified` controls how an import statement should use the qualified keyword. This is a TOML table
with three boolean keys which all default to false:
- `qualifiedPre` whether or not an import can be written with the qualifier before the module name, such as
```haskell
import qualified UnliftIO`
```
- `qualifiedPost` whether or not an import can be written with the qualifier after the module name, such as
```haskell
import UnliftIO qualified`
```

- `unqualified` whether or not an import can be written without the qualified keyword entirely, such as
```haskell
import UnliftIO`
```

####### alias

Required: No

`alias` controls what alias can be used as part of an import scheme. It is a string value of what is allowed for this particular `importScheme`. This is the part of an import that comes after the `as` keyword, such as

```haskell
import UnliftIO as Foo`
```

####### safe

Required: No

`safe` is a boolean field that controls if the import is required to use the `safe` keyword. Most
users are not expected to need this option.

```haskell
import safe Data.Bool
```
####### packageQualified

Required: No

`packageQualified` is a boolean field that controls if the import is required to use the package
qualified imports feature. Most users are not expected to need this option.

```haskell
import "unliftio" UnliftIO
```
##### encapsulatedTrees

Required: Yes

The `encapsulatedTrees` option lets you declare that the root of a module tree is effectively a
public interface that any modules outside the tree should be using. `henforcer` will report an error
if any module outside the tree attempts to import a module from inside the encapsulated tree.

This option is a TOML array of strings which are module names.

```toml
[forAnyModule]
encapsulatedTrees = [ "Service.ThirdPartyPetsSite" ]
```

##### treeDependencies

Required: No

The `treeDependencies` options lets you declare that one module tree depends on other
trees. Declaring such a dependency tells `henforcer` that you don't want the dependency targetso to
import anything from the dependent tree, which would cause a backwards dependency rendering the two
module trees logically inseparable.  `henforcer` will report any imports causing a backward
dependency as an error.

This option has a value of a TOML array of tables with two keys:
- `moduleTree` has a value of string representing the tree which depends on others.
- `dependencies` has a value of an array of strings, which are the trees that the one specified by
  `moduleTree`dependes upon.


```toml
[forAnyModule]
treeDependencies = [ {moduleTree = "PetStore", dependencies = ["Service"]} ]
```

##### maximumExportsPlusHeaderUndocumented

Required: No

`maximumExportsPlusHeaderUndocumented` is an non-negative integer to enforce a maximum number of
exported items, along with the module header, from a module that may be missing Haddock
documentation. This allows a codebase or module that is partially annotated to gradually dial the
option down over time as Haddock coverage increases.


```toml
[forAnyModule]
maximumExportsPlusHeaderUndocumented = 1
```
##### minimumExportsPlusHeaderDocumented

Required: No

`minimumExportsPlusHeaderDocumented` is an non-negative integer to enforce a minimum number of
exported items, along with the module header, from a module that must have Haddock
documentation. This allows a codebase or module that is partially documented to continue to have at
least as much Haddock coverage.

```toml
[forAnyModule]
minimumExportsPlusHeaderDocumented = 1
```
##### maximumExportsWithoutSince

Required: No

`maximumExportsWithoutSince` is an non-negative integer to enforce a maximum number of exported
items from a module that can be lacking the `@since` annotation in their Haddock. This allows a
codebase or module that is partially annotated to gradually dial the option down over time as
coverage for the `@since` annotation increases.

```toml
[forAnyModule]
maximumExportsWithoutSince = 1
```
##### minimumExportsWithSince

Required: No

`minimumExportsWithSince` is an non-negative integer to enforce a minimum number of exported items
from a module that must have in their Haddock the `@since` annotation. This allows a codebase or
module that is partially annotated to continue to have at least as much coverage for the `@since`
annotations.

```toml
[forAnyModule]
minimumExportsWithSince = 1
```
##### moduleHeaderCopyrightMustExistNonEmpty

Required: Yes

`moduleHeaderCopyrightMustExistNonEmpty` is a boolean that determines if the `Haddock` module header
field of `Copyright` must be populated.

```toml
[forAnyModule]
moduleHeaderCopyrightMustExistNonEmpty = false
```
##### moduleHeaderDescriptionMustExistNonEmpty

Required: Yes

`moduleHeaderDescriptionMustExistNonEmpty` is a boolean that determines if the `Haddock` module
header field of `Description` must be populated.

```toml
[forAnyModule]
moduleHeaderDescriptionMustExistNonEmpty = false
```
##### moduleHeaderLicenseMustExistNonEmpty

Required: Yes

`moduleHeaderLicenseMustExistNonEmpty` is a boolean that determines if the `Haddock` module header
field of `License` must be populated.

```toml
[forAnyModule]
moduleHeaderLicenseMustExistNonEmpty = false
```
##### moduleHeaderMaintainerMustExistNonEmpty

Required: Yes

`moduleHeaderMaintainerMustExistNonEmpty` is a boolean that determines if the `Haddock` module
header field of `Maintainer` must be populated.

```toml
[forAnyModule]
moduleHeaderMaintainerMustExistNonEmpty = false
```
#### forSpecifiedModules

`forSpecifiedModules` is a top level array of tables for specifying checks that apply _only_ to a
given module. Any checks specified here are _more_ specific than those in `forAnyModule` and as such
will have precedence.

```toml
[[forSpecifiedModules]]
module ="PetStore.Store"
allowedOpenUnaliasedImports = 2
moduleHeaderCopyrightMustExistNonEmpty = true
```
