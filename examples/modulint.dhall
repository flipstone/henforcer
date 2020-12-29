{ treeDependencies =
  [ { moduleTree = "PetStore"
    , dependencies = ["Service"]
    }
  ]

, encapsulatedTrees =
  [ "Service.ThirdPartyPetsSite"
  ]
}
