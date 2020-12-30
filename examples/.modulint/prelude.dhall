let
  -- A TreeName is a dot-delimited module-like name corresponding to a
  -- location in the directory tree. E.G. "Control.Monad" corresponds to
  -- all modules beginning with "Control.Monad.", as well as "Control.Monad"
  -- itself. TreeNames cannot be the empty string.
  TreeName = Text

let
  Dependency =
    { moduleTree    : TreeName
    , dependencies  : List TreeName
    }

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
    }

in
  { Type = Config
  , default =
    { treeDependencies  = [] : List Dependency
    , encapsulatedTrees = [] : List TreeName
    }
  }
