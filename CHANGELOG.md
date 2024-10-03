## next version — ????-??-??

### Patch

* Make it build with GHC `9.10`.

## 0.4.0.1 — 2024-04-05

### Patch

* Add explicit Haskell names to the `#enum MDB_cursor_op`. This is a workaround
  for https://github.com/haskell/hsc2hs/pull/89, because not having explicit
  Haskell names prevents cross-compilation via ASM (for now).

## 0.4.0.0 — 2024-04-04

### Breaking

* Stricter bounds on `base` dependency: `base >= 4.14 && < 4.20`.
* Stricter bounds on `lmdb` system package dependency: `lmdb >= 0.9 && <0.10`
* Major, minor and patch field versions in `LMDB_Version` changed from `Int` to `CLong`.
* Remove the `Ord` instance for `LMDB_Version`.
* Change `Int` to `CInt` in the `e_code` field in `LMDB_Error`.
* Remove the `Ord` instance for `LMDB_Error`.
* Remove the `Ix` instance for `MDB_ErrCode`.
* Remove `MDB_txnid`.
* Change the type of the `mv_data` field in `MDB_val` from `Ptr Word8` to `Ptr
  ()`.
* Remove the `Ord` instances for `MDB_stat`.
* Remove the `Ord` instance for `MDB_envinfo`.
* `me_last_txnid` now returns `CSize` instead of `MDB_txnid`.
* Remove the `Ix` instance for` MDB_EnvFlag`.
* Remove the `Ix` instance for `MDB_DbFlag`.
* Remove the `Ix` instance for `MDB_cursor_op`.
* Remove the `Ix` instance for `MDB_WriteFlag`.

### Non-breaking

* No longer depends on the `array` package.
* Add an `Enum` instance for `MDB_ErrCode`.
* Add an `Enum` instance for `MDB_EnvFlag`.
* Add an `Enum` instance for `MDB_DbFlag`.

## 0.3.0.0 — 2023-02-17

First release of `lmdb` fork. See
[INPUT-OUTPUT-FORK.md](./INPUT-OUTPUT-FORK.md).
