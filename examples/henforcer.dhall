let Hen = ./.henforcer/package.dhall

in  Hen.Config::{
    , treeDependencies =
      [ { moduleTree = "PetStore", dependencies = [ "Service" ] } ]
    , encapsulatedTrees = [ "Service.ThirdPartyPetsSite" ]
    , allowedQualifications = toMap
        { Prelude = Hen.qualifiedEither # [ Hen.unqualified ]
        , UnliftIO =
            Hen.onlyPackageQualified "unliftio" (Hen.qualifiedEitherAs "Foo")
        , `Data.List` = [ Hen.qualifiedAs "L" ]
        , `Data.Bool` = Hen.onlySafe [ Hen.unqualified ]
        , `Data.Text` =
            Hen.onlyPackageQualified "text" (Hen.qualifiedEitherAs "T")
        , `PetStore.Pet.Model` =
          [ Hen.qualifiedPostAs "PetModel", Hen.unqualified ]
        }
    , defaultAllowedOpenUnaliasedImports =
        Hen.defaultMaxAllowedOpenUnaliasedImports 1
    , perModuleOpenUnaliasedImports = toMap { `PetStore.Store` = 2 }
    , allowedAliasUniqueness = Hen.allAliasesUniqueExcept [ "M" ]
    }
