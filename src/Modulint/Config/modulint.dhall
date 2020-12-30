-- This loads the Config type from prelude.dhall and uses
-- record extension to override the default values with your
-- own settings. Consult the prelude.dhall file for a reference
-- to settings and the provided defaults.
(./.modulint/prelude.dhall)::
  { sourcePaths = [ "src" ]
  }
