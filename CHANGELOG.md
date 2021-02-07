# Changelog

### 0.4.0 (2021-02-07)

* Updated for Hedgehog 0.10.0
* `Gen.auto` now uses Hedgehog’s new `DateTime` and `DateTimeOffset` generators

### 0.3.0 (2021-01-06)

* Auto-generator now supports overrides; use`GenX.defaults |> AutoGenConfig.addGenerator ...` to add your custom generators
* **Breaking:** All the default `Gen<_>` members are removed from `AutoGenConfig`. Where you previously used copy-and-update syntax like `{ GenX.defaults with Int = ... }` to override default primitive auto-generators, you must now use `GenX.defaults |> AutoGenConfig.addGenerator ...` (the same API now used for overriding the generator for any type, including user-defined types).
* Fixed decimal auto-generator generating out-of-bounds values
* Added support for `single` to auto-generator

### 0.2.3 (2020-07-27)

* Updated dependencies

### 0.2.2 (2019-10-11)

* Improved URI generation

### 0.2.0 (2019-09-26)

* Added enum support to GenX.auto

### 0.1.5 (2019-08-09)

* Added GenX.uri and added URI support to GenX.auto

### 0.1.4 (2019-07-10)

* Updated GenX.auto to support uint16, uint32, and uint64

### 0.1.1 (2018-09-15)

* Updated to support TypeShape 5.1.0

### 0.1.0 (2018-02-26)

* Initial release