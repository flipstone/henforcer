let
  Ml = ./.modulint/package.dhall
in
  Ml.Config::
  { sourcePaths = [ "." ]

  , treeDependencies =
    [ { moduleTree = "PetStore"
      , dependencies = ["Service"]
      }
    ]

  , encapsulatedTrees =
    [ "Service.ThirdPartyPetsSite"
    ]

  , allowedQualifications =
    toMap
      { `Prelude` = [ Ml.unqualified ]
      , `PetStore.Pet.Model` = [ Ml.qualifiedAs "PetModel", Ml.unqualified ]
      }
  }
