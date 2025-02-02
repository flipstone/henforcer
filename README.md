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

_Note: `henforcer` requires a handful of packages that are not on stackage as of lts-23.6. The following is likely to needed in the `extra-deps` section of your `stack.yaml`

```
# henforcer needs pollock
- pollock-0.1.0.1
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

`henforcer` is a GHC plugin, thus it is executed during compilation. To enable the plugin during
compilation with `cabal` or `stack`, add `henforcer` as a dependency in your cabal file or
package.yaml as applicable and add `-fplugin Henforcer` to `ghc-flags`. Specifying plugin options is
done with `ghc-flags` as well. Currently only supported is the path to the configuration, which if
it is at `foo/bar/henforcer.toml` this would be as
`"-fplugin-opt=Henforcer:-cfoo/bar/henforcer.toml"`.

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

| fieldName                                  | type                          | required | description                                                                                                                                                                                                                                                                               |
|--------------------------------------------|-------------------------------|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `allowedAliasUniqueness`                   | AllowedAliasUniqueness        | no       | Specifies either that all aliases in the module being compiled are unique except for some, or that a given set of aliases is unique but others may be duplicated.                                                                                                                         |
| `allowedOpenUnaliasedImports`              | non-negative int              | no       | Specifies how many imports are allowed to be done without using the `qualified` keyword, or using an `alias`                                                                                                                                                                              |
| `allowedQualifications`                    | array of AllowedQualification | no       | Represents how certain modules should be imported. This can be thought of as a map of module name to a list of ways that module may be imported.                                                                                                                                          |
| `encapsulatedTrees`                        | array of string               | yes      | Lets you declare that the root of a module tree is effectively a public interface that any modules outside the tree should be using. `henforcer` will report an error if any module outside the tree attempts to import a module from inside the encapsulated tree.                       |
| `maximumExportsPlusHeaderUndocumented`     | non-negative integer          | no       | Maximum number of exported items, along with the module header, from a module that may be missing Haddock documentation.                                                                                                                                                                  |
| `minimumExportsPlusHeaderDocumented`       | non-negative integer          | no       | Minimum number of exported items, along with the module header, from a module that must have Haddock documentation.                                                                                                                                                                       |
| `maximumExportsWithoutSince`               | non-negative integer          | no       | Maximum number of exported items from a module that can be lacking the `@since` annotation in their Haddock.                                                                                                                                                                              |
| `minimumExportsWithSince`                  | non-negative integer          | no       | Minimum number of exported items from a module that must have in their Haddock the `@since` annotation.                                                                                                                                                                                   |
| `moduleHeaderCopyrightMustExistNonEmpty`   | boolean                       | no       | If the `Haddock` module header field of `Copyright` must be populated.                                                                                                                                                                                                                    |
| `moduleHeaderDescriptionMustExistNonEmpty` | boolean                       | no       | If the `Haddock` module header field of `Description` must be populated.                                                                                                                                                                                                                  |
| `moduleHeaderLicenseMustExistNonEmpty`     | boolean                       | no       | If the `Haddock` module header field of `License` must be populated.                                                                                                                                                                                                                      |
| `moduleHeaderMaintainerMustExistNonEmpty`  | boolean                       | no       | If the `Haddock` module header field of `Maintainer` must be populated.                                                                                                                                                                                                                   |
| `treeDependencies`                         | array of TreeDependency       | no       | Declares that one module tree depends on other trees. Declaring such a dependency tells `henforcer` that you don't want the dependency targets to import anything from the dependent tree, which would cause a backwards dependency rendering the two module trees logically inseparable. |

#### forSpecifiedModules

Henforcer allows for certain rules to be overridden on a module by module basis. When provided, the
most specific rule will be applied.

| fieldName                                  | type                          | required | description                                                                                                                                                       |
|--------------------------------------------|-------------------------------|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `module`                                   | string                        | yes      | `module` is a string of the module name the rules in this table will apply to.                                                                                    |
| `allowedAliasUniqueness`                   | AllowedAliasUniqueness        | no       | Specifies either that all aliases in the module being compiled are unique except for some, or that a given set of aliases is unique but others may be duplicated. |
| `allowedOpenUnaliasedImports`              | non-negative int              | no       | Specifies how many imports are allowed to be done without using the `qualified` keyword, or using an `alias`                                                      |
| `allowedQualifications`                    | array of AllowedQualification | no       | Represents how certain modules should be imported. This can be thought of as a map of module name to a list of ways that module may be imported.                  |
| `maximumExportsPlusHeaderUndocumented`     | non-negative integer          | no       | Maximum number of exported items, along with the module header, from a module that may be missing Haddock documentation.                                         |
| `minimumExportsPlusHeaderDocumented`       | non-negative integer          | no       | Minimum number of exported items, along with the module header, from a module that must have Haddock documentation.                                               |
| `maximumExportsWithoutSince`               | non-negative integer          | no       | Maximum number of exported items from a module that can be lacking the `@since` annotation in their Haddock.                                                      |
| `minimumExportsWithSince`                  | non-negative integer          | no       | Minimum number of exported items from a module that must have in their Haddock the `@since` annotation.                                                           |
| `moduleHeaderCopyrightMustExistNonEmpty`   | boolean                       | no       | If the `Haddock` module header field of `Copyright` must be populated.                                                                                            |
| `moduleHeaderDescriptionMustExistNonEmpty` | boolean                       | no       | If the `Haddock` module header field of `Description` must be populated.                                                                                          |
| `moduleHeaderLicenseMustExistNonEmpty`     | boolean                       | no       | If the `Haddock` module header field of `License` must be populated.                                                                                              |
| `moduleHeaderMaintainerMustExistNonEmpty`  | boolean                       | no       | If the `Haddock` module header field of `Maintainer` must be populated.                                                                                           |
| `rulesToIgnore`                            | RulesToIgnore                 | no       | Specifies what, if any, rules should be ignored for the given module.                                                                                             |
#### forPatternModules

Henforcer supports a limited form of using patterns to match rules against multiple modules, but not any module in a more concise way.

`forPatternModules` is an array of TOML tables. Effectively this is a map keyed by the `pattern` field.

Important items to note:
  - When determining which version of a rule to pick the definition in `forSpecifiedModules` is most
    preferred, followed by `forPatternModules` and finally `forAnyModule`.
  - If there are overlapping `pattern` keys in `forPatternModules` the first specified in the TOML will be chosen.
  - Patterns use `*` and `**`. `*` can be used to match up to the module separator `.`, where `**`
    matches across the `.` separator.

| fieldName                                  | type                          | required | description                                                                                                                                                       |
|--------------------------------------------|-------------------------------|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `pattern`                                  | string with wildcard          | yes      | `module` is a string, with wildcard support, of the module name the rules described here will apply to.                                                           |
| `allowedAliasUniqueness`                   | AllowedAliasUniqueness        | no       | Specifies either that all aliases in the module being compiled are unique except for some, or that a given set of aliases is unique but others may be duplicated. |
| `allowedOpenUnaliasedImports`              | non-negative int              | no       | Specifies how many imports are allowed to be done without using the `qualified` keyword, or using an `alias`                                                      |
| `allowedQualifications`                    | array of AllowedQualification | no       | Represents how certain modules should be imported. This can be thought of as a map of module name to a list of ways that module may be imported.                  |
| `maximumExportsPlusHeaderUndocumented`     | non-negative integer          | no       | Maximum number of exported items, along with the module header, from a module that may be missing Haddock documentation.                                          |
| `minimumExportsPlusHeaderDocumented`       | non-negative integer          | no       | Minimum number of exported items, along with the module header, from a module that must have Haddock documentation.                                               |
| `maximumExportsWithoutSince`               | non-negative integer          | no       | Maximum number of exported items from a module that can be lacking the `@since` annotation in their Haddock.                                                      |
| `minimumExportsWithSince`                  | non-negative integer          | no       | Minimum number of exported items from a module that must have in their Haddock the `@since` annotation.                                                           |
| `moduleHeaderCopyrightMustExistNonEmpty`   | boolean                       | no       | If the `Haddock` module header field of `Copyright` must be populated.                                                                                            |
| `moduleHeaderDescriptionMustExistNonEmpty` | boolean                       | no       | If the `Haddock` module header field of `Description` must be populated.                                                                                          |
| `moduleHeaderLicenseMustExistNonEmpty`     | boolean                       | no       | If the `Haddock` module header field of `License` must be populated.                                                                                              |
| `moduleHeaderMaintainerMustExistNonEmpty`  | boolean                       | no       | If the `Haddock` module header field of `Maintainer` must be populated.                                                                                           |
| `rulesToIgnore`                            | RulesToIgnore                 | no       | Specifies what, if any, rules should be ignored for the given module.                                                                                             |

#### Shared types

Below are the reused definitions between some combination of the `forAnyModule`, `forSpecifiedModules` and `forPatternModules` rules.

##### AllowedAliasUniqueness
This is allowed to take two forms that are both TOML tables.

###### First Form
This form states that all aliases in a module must be unique with an allow list for those aliases
that may be repeated.

| fieldName                | type            | required | description                                                              |
|--------------------------|-----------------|----------|--------------------------------------------------------------------------|
| `allAliasesUniqueExcept` | array of string | yes      | Aliases that are allowed to be repeated.                                 |
| `note`                   | string          | no       | User defined message to be displayed with errors for additional context. |

###### Second Form
This form states that aliases in a module may repeat with a block list for those aliases
that must be unique.

| fieldName       | type            | required | description                                                              |
|-----------------|-----------------|----------|--------------------------------------------------------------------------|
| `uniqueAliases` | array of string | yes      | Aliases that must be unique.                                             |
| `note`          | string          | no       | User defined message to be displayed with errors for additional context. |

##### AllowedQualification
| fieldName      | type                  | required | description                                                                 |
|----------------|-----------------------|----------|-----------------------------------------------------------------------------|
| `module`       | string                | yes      | `module` is a string of the module name.                                    |
| `importScheme` | array of ImportScheme | yes      | The list of specifications for each way that the given module may imported. |

##### ImportScheme
| fieldName | type      | required | description                                                                                                                                                                     |
|-----------|-----------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| qualified | Qualified | yes      | Description of ways the import can be qualified, or not.                                                                                                                        |
| `alias`   | string    | no       | Controls if and what alias can be used as part of an import scheme. This is the part of an import that comes after the `as` keyword, such as "Foo" in `import UnliftIO as Foo`. |
| `safe`    | boolean   | no       | Controls if the import is required to use the `safe` keyword. Most users are not expected to need this option.                                                                  |
| `note`    | string    | no       | User defined message to be displayed with errors for additional context.                                                                                                        |

##### Qualified

| fieldName       | type    | required | description                                                                         |
|-----------------|---------|----------|-------------------------------------------------------------------------------------|
| `qualifiedPre`  | boolean | no       | Describes if import can be qualified prepositive like `import qualified UnliftIO`.  |
| `qualifiedPost` | boolean | no       | Describes if import can be qualified postpositive like `import UnliftIO qualified`. |
| `unqualified`   | boolean | no       | Describes if import can be unqualified like `import UnliftIO`.                      |

##### TreeDependency
| fieldName      | type            | required | description |
|----------------|-----------------|----------|-------------|
| `moduleTree`   | string          | yes      | The tree which depends on others. |
| `dependencies` | array of string | yes      | The trees which are depended upon. |
| `note`         | string          | no       | User defined message to be displayed with errors for additional context. |

##### RulesToIgnore
This is allowed to take two forms that are both TOML tables.

###### First form
| fieldName | type    | required | description                                |
|-----------|---------|----------|--------------------------------------------|
| `all`     | boolean | no       | Controls if *all* rules should be ignored. |

###### Second form
| fieldName                                  | type    | required | description                             |
|--------------------------------------------|---------|----------|-----------------------------------------|
| `allowedAliasUniqueness`                   | boolean | no       | Controls if the rule should be ignored. |
| `allowedOpenUnaliasedImports`              | boolean | no       | Controls if the rule should be ignored. |
| `allowedQualifications`                    | boolean | no       | Controls if the rule should be ignored. |
| `encapsulatedTrees`                        | boolean | no       | Controls if the rule should be ignored. |
| `maximumExportsPlusHeaderUndocumented`     | boolean | no       | Controls if the rule should be ignored. |
| `minimumExportsPlusHeaderDocumented`       | boolean | no       | Controls if the rule should be ignored. |
| `maximumExportsWithoutSince`               | boolean | no       | Controls if the rule should be ignored. |
| `minimumExportsWithSince`                  | boolean | no       | Controls if the rule should be ignored. |
| `moduleHeaderCopyrightMustExistNonEmpty`   | boolean | no       | Controls if the rule should be ignored. |
| `moduleHeaderDescriptionMustExistNonEmpty` | boolean | no       | Controls if the rule should be ignored. |
| `moduleHeaderLicenseMustExistNonEmpty`     | boolean | no       | Controls if the rule should be ignored. |
| `moduleHeaderMaintainerMustExistNonEmpty`  | boolean | no       | Controls if the rule should be ignored. |
| `treeDependencies`                         | boolean | no       | Controls if the rule should be ignored. |
