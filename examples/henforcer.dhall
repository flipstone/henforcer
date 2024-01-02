let Hen = ./.henforcer/package.dhall

in  Hen.Config::{
    , forAnyModule = Hen.ForAnyModule::{
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
      , allowedAliasUniqueness = Hen.allAliasesUniqueExcept [ "M" ]
      , allowedOpenUnaliasedImports = Hen.maximumAllowed 1
      , maximumExportsUndocumented = Hen.maximumAllowed 1
      , minimumExportsDocumented = Hen.minimumAllowed 2
      , maximumExportsWithoutSince = Hen.maximumAllowed 0
      , minimumExportsWithSince = Hen.minimumAllowed 1
      , moduleHeaderCopyrightMustExistNonEmpty = True
      , moduleHeaderDescriptionMustExistNonEmpty = True
      , moduleHeaderLicenseMustExistNonEmpty = True
      }
    , forSpecifiedModule = toMap
        { `PetStore.Store` = Hen.ForSpecifiedModule::{
          , allowedOpenUnaliasedImports = Some (Hen.maximumAllowed 2)
          , moduleHeaderMaintainerMustExistNonEmpty = Some True
          }
        }
    }
