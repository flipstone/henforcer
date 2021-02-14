let
  -- A ModuleName is a dot-delimited module name corresponding to a
  -- Haskell module. E.G. "Control.Monad" ModuleNames cannot be the
  -- empty string.
  ModuleName = Text

let
  -- A TreeName is a dot-delimited module-like name corresponding to a
  -- location in the directory tree. E.G. "Control.Monad" corresponds to
  -- all modules beginning with "Control.Monad.", as well as "Control.Monad"
  -- itself. Textually it appears the same as a ModuleName, but is used to
  -- denote the tree rather than an individual module. TreeNames cannot abe
  -- the empty string.
  TreeName = Text

let
  -- A Dependency declares that one subtree of the project depends on
  -- other subtrees. This forbids any modules in the subtrees listed in
  -- `dependencies` from importing any module contained within the subtree
  -- indicated by `moduleTree`
  Dependency =
    { moduleTree    : TreeName
    , dependencies  : List TreeName
    }

let
  -- Indicates whether an allowed qualification is quailfied or unqualified
  Qualification =
    < Qualified
    | Unqualified
    >

let
  -- Indicates whether an allowed qualification has an alias, and if so what
  -- the alias must be.
  Alias =
    < WithAlias : ModuleName
    | WithoutAlias
    >

let
  -- Describes an allowed qualification scheme for a module when it is imported.
  -- When allowed qualifications are declared for a module, any import of that
  -- module must match one of the `AllowedQualifications` given for it in the
  -- configuration.
  AllowedQualification =
    { qualification : Qualification
    , alias : Alias
    }

let
  -- Build an `AllowedQualification` for unqualified imports without an alias
  unqualified =
    { qualification = Qualification.Unqualified
    , alias = Alias.WithoutAlias
    }

let
  -- Build an `AllowedQualification` for unqualified imports that must have the
  -- provided alias
  unqualifiedAs =
    \(aliasName : Text) ->
      { qualification = Qualification.Unqualified
      , alias = Alias.WithAlias aliasName
      }

let
  -- Build an `AllowedQualification` for qualified imports without an alias
  qualified =
    { qualification = Qualification.Qualified
    , alias = Alias.WithoutAlias
    }

let
  -- Build an `AllowedQualification` for qualified imports that must have the
  -- provided alias
  qualifiedAs =
    \(aliasName : Text) ->
      { qualification = Qualification.Qualified
      , alias = Alias.WithAlias aliasName
      }

let
  AllowedQualificationMap =
    List { mapKey : ModuleName, mapValue : List AllowedQualification }

let
  Config =
    { -- Paths that modulint should search for Haskell files when run
      -- You most likely want to override this.
      sourcePaths : List Text

      -- A Dependency declares that one subtree of modules depends on another,
      -- forbidding the dependency targets from importing any module from the
      -- tree declaring the dependency.
    , treeDependencies  : List Dependency

      -- Modules inside encapsulated trees may not be directly imported from
      -- outside the tree. Only the root moduleo of the tree may be imported
      -- by outside modules.
    , encapsulatedTrees : List TreeName

      -- A map of module names to the allowed qualification schemes that
      -- map be used when the module is imported. If a module appears in this
      -- map and is imported in a form that does not match one of the allowed
      -- forms given here, modulint with report it as an error.
      -- You can build the map explicitly using `mapKey` and `mapValue` for
      -- the `ModuleName` and list of `AllowedQualification` respectively, or
      -- use Dhall's `toMap` to write the key/values pairs as a record. Note:
      -- If you `toMap`, you will neded to quote module names with backticks to
      -- permit a `.` to appear in them.
    , allowedQualifications : AllowedQualificationMap
    }

in
  { Config =
    { Type = Config
    , default =
      { treeDependencies      = [] : List Dependency
      , encapsulatedTrees     = [] : List TreeName
      , allowedQualifications = toMap {=} : AllowedQualificationMap
      }
    }

  , Qualification = Qualification
  , Alias = Alias
  , AllowedQualification = AllowedQualification
  , AllowedQualificationMap = AllowedQualificationMap
  , unqualified = unqualified
  , unqualifiedAs = unqualifiedAs
  , qualified = qualified
  , qualifiedAs = qualifiedAs
  , Dependency = Dependency
  , TreeName = TreeName
  , ModuleName = ModuleName
  }
