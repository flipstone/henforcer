# Changelog for Henforcer

## Unreleased changes

* No longer consider synthetic imports, such as the Backpack signatures that a module depends on
  transitively. GHC injects these without a real source location, and their presence could make it
  impossible to write a working `allowedQualifications` rule for a signature.

* Fix `packageQualified` never matching a package whose name contains a hyphen. The package name is
  now taken as everything up to the version in the unit id, rather than everything up to the first
  hyphen.

* Only require GHC to parse Haddock comments when the configuration contains a documentation rule,
  and only gather the documentation of a module when one of its rules is enforced. Projects with no
  documentation rules no longer pay for either.

## 1.0.0.1 -- 2026-02-08

* Fix for missing test config file in distributed tarball.

## 1.0.0.0 -- 2026-01-27

* First released version after many years of internal use.
