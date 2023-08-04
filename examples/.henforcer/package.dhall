let
    -- An inlined version of List/map to allow running with no network connection
    map
    : forall (a : Type) -> forall (b : Type) -> (a -> b) -> List a -> List b
    = \(a : Type) ->
      \(b : Type) ->
      \(f : a -> b) ->
      \(xs : List a) ->
        List/build
          b
          ( \(list : Type) ->
            \(cons : b -> list -> list) ->
              List/fold a xs list (\(x : a) -> cons (f x))
          )

let
    -- A ModuleName is a dot-delimited module name corresponding to a
    -- Haskell module. E.G. "Control.Monad" ModuleNames cannot be the
    -- empty string.
    ModuleName =
      Text

let
    -- A TreeName is a dot-delimited module-like name corresponding to a
    -- location in the directory tree. E.G. "Control.Monad" corresponds to
    -- all modules beginning with "Control.Monad.", as well as "Control.Monad"
    -- itself. Textually it appears the same as a ModuleName, but is used to
    -- denote the tree rather than an individual module. TreeNames cannot abe
    -- the empty string.
    TreeName =
      Text

let
    -- A Dependency declares that one subtree of the project depends on
    -- other subtrees. This forbids any modules in the subtrees listed in
    -- `dependencies` from importing any module contained within the subtree
    -- indicated by `moduleTree`
    Dependency =
      { moduleTree : TreeName, dependencies : List TreeName }

let
    -- Indicates whether an allowed qualification is quailfied pre, qualified post, or unqualified
    Qualification =
      < QualifiedPre | QualifiedPost | Unqualified >

let
    -- Indicates whether an allowed qualification has an alias, and if so what
    -- the alias must be.
    Alias =
      < WithAlias : ModuleName | WithoutAlias >

let
    -- Indicates whethter an allowed import is only imported if it can be done so safely.
    Safe =
      < WithSafe | WithoutSafe >

let
    -- Describes an allowed qualification scheme for a module when it is imported.
    -- When allowed qualifications are declared for a module, any import of that
    -- module must match one of the `AllowedQualification`s given for it in the
    -- configuration.
    AllowedQualification =
      { qualification : Qualification, alias : Alias, safe : Safe }

let
    -- Build an `AllowedQualification` for unqualified imports without an alias
    unqualified =
      { qualification = Qualification.Unqualified
      , alias = Alias.WithoutAlias
      , safe = Safe.WithoutSafe
      }

let
    -- Build an `AllowedQualification` for unqualified imports that must have the
    -- provided alias
    unqualifiedAs =
      \(aliasName : Text) ->
        { qualification = Qualification.Unqualified
        , alias = Alias.WithAlias aliasName
        , safe = Safe.WithoutSafe
        }

let
    -- Build an `AllowedQualification` for qualified imports without an alias
    qualified =
      { qualification = Qualification.QualifiedPre
      , alias = Alias.WithoutAlias
      , safe = Safe.WithoutSafe
      }

let
    -- Build an `AllowedQualification` for qualified imports that must have the
    -- provided alias
    qualifiedAs =
      \(aliasName : Text) ->
        { qualification = Qualification.QualifiedPre
        , alias = Alias.WithAlias aliasName
        , safe = Safe.WithoutSafe
        }

let
    -- Build an `AllowedQualification` for qualified post imports without an alias
    qualifiedPost =
      { qualification = Qualification.QualifiedPost
      , alias = Alias.WithoutAlias
      , safe = Safe.WithoutSafe
      }

let
    -- Build an `AllowedQualification` for qualified post imports that must have the
    -- provided alias
    qualifiedPostAs =
      \(aliasName : Text) ->
        { qualification = Qualification.QualifiedPost
        , alias = Alias.WithAlias aliasName
        , safe = Safe.WithoutSafe
        }

let
    -- Build a list `AllowedQualification` for qualified pre or qualified post imports without
    -- an alias
    qualifiedEither =
      [ { qualification = Qualification.QualifiedPre
        , alias = Alias.WithoutAlias
        , safe = Safe.WithoutSafe
        }
      , { qualification = Qualification.QualifiedPost
        , alias = Alias.WithoutAlias
        , safe = Safe.WithoutSafe
        }
      ]

let
    -- Build a list of `AllowedQualification` for qualified pre or qualified post imports that
    -- must have the provided alias
    qualifiedEitherAs =
      \(aliasName : Text) ->
        [ { qualification = Qualification.QualifiedPre
          , alias = Alias.WithAlias aliasName
          , safe = Safe.WithoutSafe
          }
        , { qualification = Qualification.QualifiedPost
          , alias = Alias.WithAlias aliasName
          , safe = Safe.WithoutSafe
          }
        ]

let
    -- Mark an import as requiring safe
    setWithSafe =
      \(import : AllowedQualification) -> import // { safe = Safe.WithSafe }

let
    -- Modify a list of `AllowedQualification` to only allow safe imports
    onlySafe =
      \(imports : List AllowedQualification) ->
        map AllowedQualification AllowedQualification setWithSafe imports

let AllowedQualificationMap =
      List { mapKey : ModuleName, mapValue : List AllowedQualification }

let
    -- A key-value list of 'ModuleName' and numbers that represent the number of imports that are
    -- allowed to be imported completely open with no alias.
    OpenUnaliasedImportMap =
      List { mapKey : ModuleName, mapValue : Natural }

let
    -- Describes if there is a maximum number of open, unaliased, imports that should apply to all
    -- modules. Note this is superceded if a per module max is specified for a given module.
    DefaultAllowedOpenUnaliasedImports =
      < DefaultMaximum : Natural | NoDefaultMaximum >

let -- Build a default max in a more ergonomic way in configurations.
    defaultMaxAllowedOpenUnaliasedImports =
      \(maxAllowed : Natural) ->
        DefaultAllowedOpenUnaliasedImports.DefaultMaximum maxAllowed

let Config =
      { treeDependencies : List Dependency
      , encapsulatedTrees : List TreeName
      , allowedQualifications : AllowedQualificationMap
      , defaultAllowedOpenUnaliasedImports : DefaultAllowedOpenUnaliasedImports
      , perModuleOpenUnaliasedImports : OpenUnaliasedImportMap
      }

in  { Config =
      { Type = Config
      , default =
        { treeDependencies = [] : List Dependency
        , encapsulatedTrees = [] : List TreeName
        , allowedQualifications = toMap {=} : AllowedQualificationMap
        , defaultAllowedOpenUnaliasedImports =
            DefaultAllowedOpenUnaliasedImports.NoDefaultMaximum
        , perModuleOpenUnaliasedImports = toMap {=} : OpenUnaliasedImportMap
        }
      }
    , Dependency
    , TreeName
    , Qualification
    , Alias
    , Safe
    , AllowedQualification
    , unqualified
    , unqualifiedAs
    , qualified
    , qualifiedAs
    , qualifiedPost
    , qualifiedPostAs
    , qualifiedEither
    , qualifiedEitherAs
    , setWithSafe
    , onlySafe
    , AllowedQualificationMap
    , defaultMaxAllowedOpenUnaliasedImports
    }
