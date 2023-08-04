--| This loads the Henforcer package that was installed when you ran
--  henforcer --init aliased as `Hen` and the record auto-complete operator
--  `::` to construct a Henforcer Config value with all the default values.
--  You can add you configuration values below as more fields. Consult the package.dhall file for a
--  reference to settings and the provided defaults.
let Hen = ./.henforcer/package.dhall in Hen.Config::{}
