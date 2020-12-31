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

  , qualificationRules =
    toMap
      { `Prelude` = Ml.QualificationRule.Forbidden
      , `PetStore.Pet.Model` = Ml.QualificationRule.RequiredAs ["PetModel"]
      }
  }
