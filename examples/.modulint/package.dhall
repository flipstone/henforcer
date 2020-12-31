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
  -- denote the tree rather than an individual module. TreeNames cannot be
  -- the empty string.
  TreeName = Text

let
  Dependency =
    { moduleTree    : TreeName
    , dependencies  : List TreeName
    }

let
  -- An qualification rule declares if and how a module must be qualified
  -- when it is imported
  QualificationRule =
    < Forbidden
    | RequiredAs : List ModuleName
    | AllowedAs  : List ModuleName
    >

let
  QualificationRuleMap =
    List { mapKey : ModuleName, mapValue : QualificationRule }

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

      -- A map of module names to the qualification rules that apply to them.
      -- You can build the map explicitly using `mapKey` and `mapValue` for
      -- the `ModuleName` and `QualificationRule` respectively, or use Dhall's
      -- `toMap` to write the key/values pairs as a record. Note: If you
      -- `toMap`, you will neded to quote module names with backticks to
      -- permit a `.` to appear in them.
    , qualificationRules : QualificationRuleMap
    }

in
  { Config =
    { Type = Config
    , default =
      { treeDependencies    = [] : List Dependency
      , encapsulatedTrees   = [] : List TreeName
      , qualificationRules  = toMap {=} : QualificationRuleMap
      }
    }

  , QualificationRule = QualificationRule
  , Dependency = Dependency
  , TreeName = TreeName
  , ModuleName = ModuleName
  }
