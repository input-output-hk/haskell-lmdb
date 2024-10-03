{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- HLINT ignore "Use camelCase" -}

-- | This module is a thin wrapper above lmdb.h.
--
-- Provisions for performance, convenience, or safety:
--
-- * Errors are shifted to `LMDB_Error` Haskell exceptions
-- * flag fields and enums are represented with Haskell types
-- * MDB_env includes its own write mutex for Haskell's threads
-- * MDB_RESERVE operations use their own functions
-- * Databases types are divided for user-defined comparisons
-- * Boolean-option functions are divided into two functions
-- * MDB_NOTLS is added implicitly, and may not be removed
-- * unix mode is set to 0660 (user+group read-write)
--
-- Some functions come in two forms based on 'safe' vs. 'unsafe'
-- FFI bindings. Unsafe FFI bindings are unsuitable for databases
-- with user-defined comparison operations. (Though, if you plan
-- to load a database with MDB_APPEND or MDB_APPENDDUP, you can
-- use an unsafe dbi for just that portion.)
--
-- Despite these provisions, developers must still be cautious:
--
-- * FFI.MDB_val objects are invalid outside their transaction.
-- * Don't use write operations on a read-only transaction.
-- * Use 'bound threads' for write transactions.
--
-- A slightly higher level API is planned, mostly to provide safer
-- and more convenient access compared to raw FFI.MDB_val objects.
--
-- Features not implemented:
--
-- * functions directly using file handles
-- * user-defined relocation functions
-- * MDB_MULTIPLE is not currently supported (todo)
--
module Database.LMDB.Raw
    ( LMDB_Version(..), lmdb_version, lmdb_dyn_version
    , LMDB_Error(..), MDB_ErrCode(..)

    , MDB_env
    , MDB_dbi, MDB_dbi'
    , MDB_txn
    , MDB_cursor, MDB_cursor'

    , FFI.MDB_val(..)
    , FFI.MDB_stat, FFI.ms_psize, FFI.ms_depth, FFI.ms_branch_pages, FFI.ms_leaf_pages, FFI.ms_overflow_pages, FFI.ms_entries
    , FFI.MDB_envinfo, FFI.me_mapaddr, FFI.me_mapsize, FFI.me_last_pgno, FFI.me_last_txnid, FFI.me_maxreaders, FFI.me_numreaders
    , FFI.MDB_cmp_func, FFI.wrapCmpFn
    , MDB_EnvFlag(..), MDB_DbFlag(..)
    , MDB_cursor_op(..)

    , MDB_WriteFlag(..), MDB_WriteFlags, compileWriteFlags
    --, MDB_cursor_op(..)

    -- * Environment Operations
    , mdb_env_create
    , mdb_env_open
    , mdb_env_copy
    , mdb_env_stat
    , mdb_env_info
    , mdb_env_sync, mdb_env_sync_flush
    , mdb_env_close
    , mdb_env_set_flags, mdb_env_unset_flags
    , mdb_env_get_flags
    , mdb_env_get_path
    , mdb_env_set_mapsize
    , mdb_env_set_maxreaders
    , mdb_env_get_maxreaders
    , mdb_env_set_maxdbs
    , mdb_env_get_maxkeysize

    -- * Transactions
    , mdb_txn_begin
    , mdb_txn_env
    , mdb_txn_commit
    , mdb_txn_abort

    -- * Databases
    , mdb_dbi_open
    , mdb_stat
    , mdb_dbi_flags
    , mdb_dbi_close
    , mdb_drop, mdb_clear
    , mdb_set_compare
    , mdb_set_dupsort

    , mdb_dbi_open'
    , mdb_stat'
    , mdb_dbi_flags'
    , mdb_dbi_close'
    , mdb_drop', mdb_clear'

    -- * Basic Key-Value Access
    , mdb_get, mdb_put, mdb_del, mdb_reserve
    , mdb_get', mdb_put', mdb_del', mdb_reserve'

    -- * Database key and value Comparisons
    , mdb_cmp, mdb_dcmp
    , mdb_cmp', mdb_dcmp'

    -- * Cursors
    , mdb_cursor_open
    , mdb_cursor_get
    , mdb_cursor_put
    , mdb_cursor_put_ptr
    , mdb_cursor_del
    , mdb_cursor_close
    , mdb_cursor_txn
    , mdb_cursor_dbi
    , mdb_cursor_count

    , mdb_cursor_open'
    , mdb_cursor_get'
    , mdb_cursor_put'
    , mdb_cursor_put_ptr'
    , mdb_cursor_del'
    , mdb_cursor_close'
    , mdb_cursor_txn'
    , mdb_cursor_dbi'
    , mdb_cursor_count'

    -- * Misc
    , mdb_reader_list
    , mdb_reader_check

    , mdb_txn_reset
    , mdb_txn_renew

    , withKVPtrs
    , withKVOptPtrs
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Function      (on)
import           Data.IORef
import qualified Data.List          as L
import           Data.Maybe         (isNothing)
import           Data.Typeable
import qualified Database.LMDB.FFI  as FFI
import           Foreign
import           Foreign.C

-- Haskell seems to have difficulty inferring the `Ptr CInt` from
-- the FFI.mdb_version call. (This seriously annoys me.)
_peekCInt :: Ptr CInt -> IO CInt
_peekCInt = peek

_peekCUInt :: Ptr CUInt -> IO CUInt
_peekCUInt = peek

-- | Version information for LMDB. Two potentially different versions
-- can be obtained: lmdb_version returns the version at the time of
-- binding (via C preprocessor macros) and lmdb_dyn_version returns a
-- version for the bound library.
--
-- These bindings to Haskell will refuse to open the database when
-- the dynamic version of LMDB is different in the major or minor
-- fields.
data LMDB_Version = LMDB_Version
    { v_major :: {-# UNPACK #-} !CLong
    , v_minor :: {-# UNPACK #-} !CLong
    , v_patch :: {-# UNPACK #-} !CLong
    , v_text  :: !String
    } deriving (Eq, Show)

-- | Version of LMDB when the Haskell-LMDB binding was compiled.
lmdb_version :: LMDB_Version
lmdb_version = LMDB_Version
    { v_major = FFI.mdbVersionMajor
    , v_minor = FFI.mdbVersionMinor
    , v_patch = FFI.mdbVersionPatch
    , v_text  =
        -- #const_str is not supported when cross compiling
        -- #const_str MDB_VERSION_STRING
        "LMDB "
          <> show (v_major lmdb_version) <> "."
          <> show (v_minor lmdb_version) <> "."
          <> show (v_patch lmdb_version)
    }


-- | Version of LMDB linked to the current Haskell process.
lmdb_dyn_version :: IO LMDB_Version
lmdb_dyn_version =
    let szInt = sizeOf (undefined :: CInt) in
    allocaBytes (3 * szInt) $ \ pMajor -> do
        let pMinor = pMajor `plusPtr` szInt
        let pPatch = pMinor `plusPtr` szInt
        cvText <- FFI.mdb_version pMajor pMinor pPatch
        vMajor <- fromIntegral <$> _peekCInt pMajor
        vMinor <- fromIntegral <$> _peekCInt pMinor
        vPatch <- fromIntegral <$> _peekCInt pPatch
        vText  <- peekCString cvText
        return $! LMDB_Version
            { v_major = vMajor
            , v_minor = vMinor
            , v_patch = vPatch
            , v_text  = vText
            }

-- | LMDB_Error is the exception type thrown in case a function from
-- the LMDB API does not return successfully. Clients should be
-- prepared to catch exceptions from any LMDB operation.
data LMDB_Error = LMDB_Error
    { e_context     :: String
    , e_description :: String
    , e_code        :: Either CInt MDB_ErrCode
    } deriving (Eq, Show, Typeable)
instance Exception LMDB_Error

-- | Opaque structure for LMDB environment.
--
-- The environment additionally contains an MVar to enforce at most
-- one lightweight Haskell thread is writing at a time. This is
-- necessary so long as LMDB uses a long-lived mutex for writes, as
-- in v0.9.10.
--
data MDB_env = MDB_env
    { _env_ptr   :: {-# UNPACK #-} !(Ptr FFI.MDB_env) -- opaque pointer to LMDB object
    , _env_wlock :: {-# UNPACK #-} !(MVar ThreadId) -- write lock
    }

-- | Opaque structure for LMDB transaction.
data MDB_txn = MDB_txn
    { _txn_ptr :: {-# UNPACK #-} !(Ptr FFI.MDB_txn)
    , _txn_env :: !MDB_env -- environment that owns this transaction.
    , _txn_rw  :: !Bool -- is this a read-write transaction? (vs read-only)
    , _txn_p   :: !(Maybe MDB_txn) -- parent transaction, if any
    }

-- | Handle for a database in the environment.
newtype MDB_dbi = MDB_dbi { _dbi :: FFI.MDB_dbi }

-- | Opaque structure for LMDB cursor.
data MDB_cursor = MDB_cursor
    { _crs_ptr :: {-# UNPACK #-} !(Ptr FFI.MDB_cursor)
    , _crs_dbi :: {-# UNPACK #-} !MDB_dbi
    , _crs_txn :: !MDB_txn
    }

-- | Handle for a database in the environment.
--
-- This variation is associated with 'unsafe' FFI calls, with reduced
-- overhead but no user-defined comparisons. I expect most code using
-- LMDB could use this variation.
newtype MDB_dbi' = MDB_dbi' { _dbi' :: FFI.MDB_dbi }

-- | Opaque structure for a cursor on an MDB_dbi' object. Cursors
-- in this case also use the 'unsafe' FFI calls.
data MDB_cursor' = MDB_cursor'
    { _crs_ptr' :: {-# UNPACK #-} !(Ptr FFI.MDB_cursor)
    , _crs_dbi' :: {-# UNPACK #-} !MDB_dbi'
    , _crs_txn' :: !MDB_txn
    }

-- | Environment flags from lmdb.h
--
-- Note: MDB_NOTLS is implicit and enforced for this binding.
data MDB_EnvFlag
    = MDB_FIXEDMAP
    | MDB_NOSUBDIR
    | MDB_NOSYNC
    | MDB_RDONLY
    | MDB_NOMETASYNC
    | MDB_WRITEMAP
    | MDB_MAPASYNC
    --  | MDB_NOTLS
    | MDB_NOLOCK
    | MDB_NORDAHEAD
    | MDB_NOMEMINIT
    deriving (Eq, Ord, Bounded, Enum, Show)

fromEnvFlag :: MDB_EnvFlag -> CUInt
fromEnvFlag = \case
    MDB_FIXEDMAP   -> FFI.mdbFixedmap
    MDB_NOSUBDIR   -> FFI.mdbNosubdir
    MDB_NOSYNC     -> FFI.mdbNosync
    MDB_RDONLY     -> FFI.mdbRdonly
    MDB_NOMETASYNC -> FFI.mdbNometasync
    MDB_WRITEMAP   -> FFI.mdbWritemap
    MDB_MAPASYNC   -> FFI.mdbMapasync
    -- MDB_NOTLS      -> FFI.mdbNotls
    MDB_NOLOCK     -> FFI.mdbNolock
    MDB_NORDAHEAD  -> FFI.mdbNordahead
    MDB_NOMEMINIT  -> FFI.mdbNomeminit

compileEnvFlags :: [MDB_EnvFlag] -> CUInt
compileEnvFlags = L.foldl' (\fls fl -> fls .|. fromEnvFlag fl) 0

hasEnvFlag :: CUInt -> MDB_EnvFlag -> Bool
hasEnvFlag fls target = fls == (fromEnvFlag target .&. fls)

decompileEnvFlags :: CUInt -> [MDB_EnvFlag]
decompileEnvFlags fls = L.filter (hasEnvFlag fls) [minBound .. maxBound]

data MDB_DbFlag
    = MDB_REVERSEKEY
    | MDB_DUPSORT
    | MDB_INTEGERKEY
    | MDB_DUPFIXED
    | MDB_INTEGERDUP
    | MDB_REVERSEDUP
    | MDB_CREATE
    deriving (Eq, Ord, Bounded, Enum, Show)

fromDbFlag :: MDB_DbFlag -> CUInt
fromDbFlag = \case
    MDB_REVERSEKEY -> FFI.mdbReversekey
    MDB_DUPSORT -> FFI.mdbDupsort
    MDB_INTEGERKEY -> FFI.mdbIntegerkey
    MDB_DUPFIXED -> FFI.mdbDupfixed
    MDB_INTEGERDUP -> FFI.mdbIntegerdup
    MDB_REVERSEDUP -> FFI.mdbReversedup
    MDB_CREATE -> FFI.mdbCreate

hasDbFlag :: CUInt -> MDB_DbFlag -> Bool
hasDbFlag fls target = fls == (fromDbFlag target .&. fls)

compileDBFlags :: [MDB_DbFlag] -> CUInt
compileDBFlags = L.foldl' (\fls fl -> fls .|. fromDbFlag fl) 0

decompileDBFlags :: CUInt -> [MDB_DbFlag]
decompileDBFlags fls = L.filter (hasDbFlag fls) [minBound .. maxBound]

data MDB_WriteFlag
    = MDB_NOOVERWRITE
    | MDB_NODUPDATA
    | MDB_CURRENT
    --  | MDB_RESERVE  -- (needs dedicated function)
    | MDB_APPEND
    | MDB_APPENDDUP
    --  | MDB_MULTIPLE -- (needs special handling)
    deriving (Eq, Ord, Bounded, Show)


fromWriteFlag :: MDB_WriteFlag -> CUInt
fromWriteFlag = \case
    MDB_NOOVERWRITE -> FFI.mdbNooverwrite
    MDB_NODUPDATA   -> FFI.mdbNodupdata
    MDB_CURRENT     -> FFI.mdbCurrent
    -- MDB_RESERVE     -> FFI.mdbReserve
    MDB_APPEND      -> FFI.mdbAppend
    MDB_APPENDDUP   -> FFI.mdbAppenddup
    -- MDB_MULTIPLE    -> FFI.mdbMultiple

-- | compiled write flags, corresponding to a [WriteFlag] list. Used
-- because writes are frequent enough that we want to avoid building
-- from a list on a per-write basis.
newtype MDB_WriteFlags = MDB_WriteFlags { unWriteFlags :: CUInt }

-- | compile a list of write flags.
compileWriteFlags :: [MDB_WriteFlag] -> MDB_WriteFlags
compileWriteFlags = MDB_WriteFlags . L.foldl' (\fls fl -> fls .|. fromWriteFlag fl) 0

data MDB_cursor_op
    = MDB_FIRST
    | MDB_FIRST_DUP
    | MDB_GET_BOTH
    | MDB_GET_BOTH_RANGE
    | MDB_GET_CURRENT
    | MDB_GET_MULTIPLE
    | MDB_LAST
    | MDB_LAST_DUP
    | MDB_NEXT
    | MDB_NEXT_DUP
    | MDB_NEXT_MULTIPLE
    | MDB_NEXT_NODUP
    | MDB_PREV
    | MDB_PREV_DUP
    | MDB_PREV_NODUP
    | MDB_SET
    | MDB_SET_KEY
    | MDB_SET_RANGE
    deriving (Eq, Ord, Bounded, Show)

fromCursorOp :: MDB_cursor_op -> FFI.MDB_cursor_op
fromCursorOp = \case
    MDB_FIRST -> FFI.mdbFirst
    MDB_FIRST_DUP -> FFI.mdbFirstDup
    MDB_GET_BOTH -> FFI.mdbGetBoth
    MDB_GET_BOTH_RANGE -> FFI.mdbGetBothRange
    MDB_GET_CURRENT -> FFI.mdbGetCurrent
    MDB_GET_MULTIPLE -> FFI.mdbGetMultiple
    MDB_LAST -> FFI.mdbLast
    MDB_LAST_DUP -> FFI.mdbLastDup
    MDB_NEXT -> FFI.mdbNext
    MDB_NEXT_DUP -> FFI.mdbNextDup
    MDB_NEXT_MULTIPLE -> FFI.mdbNextMultiple
    MDB_NEXT_NODUP -> FFI.mdbNextNodup
    MDB_PREV -> FFI.mdbPrev
    MDB_PREV_DUP -> FFI.mdbPrevDup
    MDB_PREV_NODUP -> FFI.mdbPrevNodup
    MDB_SET -> FFI.mdbSet
    MDB_SET_KEY -> FFI.mdbSetKey
    MDB_SET_RANGE -> FFI.mdbSetRange

cursorOp :: MDB_cursor_op -> FFI.MDB_cursor_op
cursorOp = fromCursorOp

-- | Error codes from MDB. Note, however, that this API for MDB will mostly
-- use exceptions for any non-successful return codes. This is mostly included
-- because I feel the binding would be incomplete otherwise.
--
-- (The MDB_SUCCESS return value is excluded.)
data MDB_ErrCode
    = MDB_KEYEXIST
    | MDB_NOTFOUND
    | MDB_PAGE_NOTFOUND
    | MDB_CORRUPTED
    | MDB_PANIC
    | MDB_VERSION_MISMATCH
    | MDB_INVALID
    | MDB_MAP_FULL
    | MDB_DBS_FULL
    | MDB_READERS_FULL
    | MDB_TLS_FULL
    | MDB_TXN_FULL
    | MDB_CURSOR_FULL
    | MDB_PAGE_FULL
    | MDB_MAP_RESIZED
    | MDB_INCOMPATIBLE
    | MDB_BAD_RSLOT
    | MDB_BAD_TXN
    | MDB_BAD_VALSIZE
    deriving (Eq, Ord, Bounded, Enum, Show)

fromErrCode :: MDB_ErrCode -> CInt
fromErrCode = \case
    MDB_KEYEXIST -> FFI.mdbKeyexist
    MDB_NOTFOUND -> FFI.mdbNotfound
    MDB_PAGE_NOTFOUND -> FFI.mdbPageNotfound
    MDB_CORRUPTED -> FFI.mdbCorrupted
    MDB_PANIC -> FFI.mdbPanic
    MDB_VERSION_MISMATCH -> FFI.mdbVersionMismatch
    MDB_INVALID -> FFI.mdbInvalid
    MDB_MAP_FULL -> FFI.mdbMapFull
    MDB_DBS_FULL -> FFI.mdbDbsFull
    MDB_READERS_FULL -> FFI.mdbReadersFull
    MDB_TLS_FULL -> FFI.mdbTlsFull
    MDB_TXN_FULL -> FFI.mdbTxnFull
    MDB_CURSOR_FULL -> FFI.mdbCursorFull
    MDB_PAGE_FULL -> FFI.mdbPageFull
    MDB_MAP_RESIZED -> FFI.mdbMapResized
    MDB_INCOMPATIBLE -> FFI.mdbIncompatible
    MDB_BAD_RSLOT -> FFI.mdbBadRslot
    MDB_BAD_TXN -> FFI.mdbBadTxn
    MDB_BAD_VALSIZE -> FFI.mdbBadValsize

_numToErrVal :: CInt -> Either CInt MDB_ErrCode
_numToErrVal res = case L.find (\code -> fromErrCode code == res) [minBound .. maxBound] of
      Nothing   -> Left res
      Just code -> Right code

_throwLMDBErrNum :: String -> CInt -> IO noReturn
_throwLMDBErrNum context errNum = do
    desc <- peekCString (FFI.mdb_strerror errNum)
    throwIO $! LMDB_Error
        { e_context = context
        , e_description = desc
        , e_code = _numToErrVal errNum
        }
{-# NOINLINE _throwLMDBErrNum #-}

-- | Allocate an environment object. This doesn't open the environment.
--
-- After creation, but before opening, please use:
--
--   mdb_env_set_mapsize
--   mdb_env_set_maxreaders
--   mdb_env_set_maxdbs
--
-- Then, just after opening, you should create a transaction to open
-- all the databases (MDB_dbi and MDB_dbi' values) your application
-- will use.
--
-- The typical use case would then involve keeping all these open
-- until your application either shuts down or crashes.
--
-- In addition to normal LMDB errors, this operation may throw an
-- MDB_VERSION_MISMATCH if the Haskell LMDB bindings don't match
-- the dynamic version. If this happens, you'll need to rebuild the
-- lmdb Haskell package.
mdb_env_create :: IO MDB_env
mdb_env_create = alloca $ \ ppEnv ->
    lmdb_validate_version_match >>
    FFI.mdb_env_create ppEnv >>= \ rc ->
    if (0 /= rc) then _throwLMDBErrNum "mdb_env_create" rc else
    MDB_env <$> peek ppEnv <*> newEmptyMVar


lmdb_validate_version_match :: IO ()
lmdb_validate_version_match =
    let vStat = lmdb_version in
    lmdb_dyn_version >>= \ vDyn ->
    unless (versionMatch vStat vDyn) $
        throwIO $! LMDB_Error
            { e_context = "lmdb_validate_version_match"
            , e_description = "Haskell bindings: " ++ show vStat
                           ++ "\tDynamic library: " ++ show vDyn
            , e_code = Right MDB_VERSION_MISMATCH
            }

-- this match function is a bit relaxed, e.g. it will accept
-- LMDB 0.9.10 with 0.9.8, so long as the first two numbers
-- match.
versionMatch :: LMDB_Version -> LMDB_Version -> Bool
versionMatch vA vB = matchMajor && matchMinor where
    matchMajor = ((==) `on` v_major) vA vB
    matchMinor = ((==) `on` v_minor) vA vB

-- | open or build a database in the filesystem. The named directory
-- must already exist and be writeable. Before opening, be sure to
-- at least apply `mdb_env_set_mapsize`.
--
-- After opening the environment, you should open the databases:
--
--    Create the environment.
--    Open a transaction.
--    Open all DBI handles the app will need.
--    Commit the transaction.
--    Use those DBI handles in subsequent transactions
--
mdb_env_open :: MDB_env -> FilePath -> [MDB_EnvFlag] -> IO ()
mdb_env_open env fp flags =
    let iFlags = FFI.mdbNotls .|. compileEnvFlags flags in
    let unix_mode = (6 * 64 + 6 * 8) in -- mode 0660, read-write for user+group
    withCString fp $ \ cfp ->
        FFI.mdb_env_open (_env_ptr env) cfp iFlags unix_mode >>= \ rc ->
        unless (0 == rc) $
            _throwLMDBErrNum "mdb_env_open" rc

-- | Copy the environment to an empty (but existing) directory.
--
-- Note: the LMDB copy operation temporarily grabs the writer mutex.
-- Unfortunately, this greatly complicates the binding to Haskell.
-- This interface, mdb_env_copy, conservatively blocks all writers
-- in the same process for the entire duration of copy.
--
-- Recommendation: Don't use this function in the same process with
-- writers. Consider use of the `mdb_copy` command line utility if
-- you need hot copies.
--
mdb_env_copy :: MDB_env -> FilePath -> IO ()
mdb_env_copy env fp =
    runInBoundThread $
    bracket_ (_lockEnv env) (_unlockEnv env) $
    withCString fp $ \ cfp ->
        FFI.mdb_env_copy (_env_ptr env) cfp >>= \ rc ->
        unless (0 == rc) (_throwLMDBErrNum "mdb_env_copy" rc)


-- | obtain statistics for environment
mdb_env_stat :: MDB_env -> IO FFI.MDB_stat
mdb_env_stat env =
    alloca $ \ pStats ->
        FFI.mdb_env_stat (_env_ptr env) pStats >>= \ rc ->
        if (0 == rc) then peek pStats else
        _throwLMDBErrNum "mdb_env_stat" rc

-- | obtain ad-hoc information about the environment.
mdb_env_info :: MDB_env -> IO FFI.MDB_envinfo
mdb_env_info env =
    alloca $ \ pInfo ->
        FFI.mdb_env_info (_env_ptr env) pInfo >>= \ rc ->
        if (0 == rc) then peek pInfo else
        _throwLMDBErrNum "mdb_env_info" rc

-- | Initiate synchronization of environment with disk. However, if
-- the MDB_NOSYNC or MDB_MAPASYNC flags are active, this won't wait
-- for the operation to finish. Cf. mdb_env_sync_flush.
mdb_env_sync :: MDB_env -> IO ()
mdb_env_sync env =
    FFI.mdb_env_sync (_env_ptr env) 0 >>= \ rc ->
    unless (0 == rc) (_throwLMDBErrNum "mdb_env_sync" rc)

-- | Force buffered writes to disk before returning.
mdb_env_sync_flush :: MDB_env -> IO ()
mdb_env_sync_flush env =
    FFI.mdb_env_sync (_env_ptr env) 1 >>= \ rc ->
    unless (0 == rc) (_throwLMDBErrNum "mdb_env_sync_flush" rc)

-- | Close the environment. The MDB_env object should not be used by
-- any operations during or after closing.
mdb_env_close :: MDB_env -> IO ()
mdb_env_close env = _lockEnv env >> FFI.mdb_env_close (_env_ptr env)

-- | Set flags for the environment.
mdb_env_set_flags :: MDB_env -> [MDB_EnvFlag] -> IO ()
mdb_env_set_flags env flags =
    FFI.mdb_env_set_flags (_env_ptr env) (compileEnvFlags flags) 1 >>= \ rc ->
    unless (0 == rc) $ _throwLMDBErrNum "mdb_env_set_flags" rc

-- | Unset flags for the environment.
mdb_env_unset_flags :: MDB_env -> [MDB_EnvFlag] -> IO ()
mdb_env_unset_flags env flags =
    FFI.mdb_env_set_flags (_env_ptr env) (compileEnvFlags flags) 0 >>= \ rc ->
    unless (0 == rc) $ _throwLMDBErrNum "mdb_env_unset_flags" rc

-- | View the current set of flags for the environment.
mdb_env_get_flags :: MDB_env -> IO [MDB_EnvFlag]
mdb_env_get_flags env = decompileEnvFlags <$> mdb_env_get_flags_u env

mdb_env_get_flags_u :: MDB_env -> IO CUInt
mdb_env_get_flags_u env = alloca $ \ pFlags ->
    FFI.mdb_env_get_flags (_env_ptr env) pFlags >>= \ rc ->
    if (0 == rc) then peek pFlags else
    _throwLMDBErrNum "mdb_env_get_flags" rc

-- | Obtain filesystem path for this environment.
mdb_env_get_path :: MDB_env -> IO FilePath
mdb_env_get_path env = alloca $ \ pPathStr ->
    FFI.mdb_env_get_path (_env_ptr env) pPathStr >>= \ rc ->
    if (0 == rc) then peekCString =<< peek pPathStr else
    _throwLMDBErrNum "mdb_env_get_path" rc

-- | Set the memory map size, in bytes, for this environment. This
-- determines the maximum size for the environment and databases,
-- but typically only a small fraction of the database is in memory
-- at any given moment.
--
-- It is not a problem to set this to a very large number, hundreds
-- of gigabytes or even terabytes, assuming a sufficiently large
-- address space. It should be set to a multiple of page size.
--
-- The default map size is 1MB, intentionally set low to force
-- developers to select something larger.
mdb_env_set_mapsize :: MDB_env -> Int -> IO ()
mdb_env_set_mapsize env nBytes =
    FFI.mdb_env_set_mapsize (_env_ptr env) (fromIntegral nBytes) >>= \ rc ->
    unless (0 == rc) (_throwLMDBErrNum "mdb_env_set_mapsize" rc)

-- | Set the maximum number of concurrent readers.
mdb_env_set_maxreaders :: MDB_env -> Int -> IO ()
mdb_env_set_maxreaders env nReaders =
    FFI.mdb_env_set_maxreaders (_env_ptr env) (fromIntegral nReaders) >>= \ rc ->
    unless (0 == rc) (_throwLMDBErrNum "mdb_env_set_maxreaders" rc)

-- | Get the maximum number of concurrent readers.
mdb_env_get_maxreaders :: MDB_env -> IO Int
mdb_env_get_maxreaders env = alloca $ \ pCount ->
    FFI.mdb_env_get_maxreaders (_env_ptr env) pCount >>= \ rc ->
    if (0 == rc) then fromIntegral <$> _peekCUInt pCount else
    _throwLMDBErrNum "mdb_env_get_maxreaders" rc

-- | Set the maximum number of named databases. LMDB is designed to
-- support a small handful of databases.
mdb_env_set_maxdbs :: MDB_env -> Int -> IO ()
mdb_env_set_maxdbs env nDBs =
    FFI.mdb_env_set_maxdbs (_env_ptr env) (fromIntegral nDBs) >>= \ rc ->
    unless (0 == rc) (_throwLMDBErrNum "mdb_env_set_maxdbs" rc)

-- | Key sizes in LMDB are determined by a compile-time constant,
-- defaulting to 511 bytes. This function returns the maximum.
mdb_env_get_maxkeysize :: MDB_env -> IO Int
mdb_env_get_maxkeysize env = fromIntegral <$> FFI.mdb_env_get_maxkeysize (_env_ptr env)

-- | Check for stale readers, and return number of stale readers cleared.
mdb_reader_check :: MDB_env -> IO Int
mdb_reader_check env =
    alloca $ \ pCount ->
    FFI.mdb_reader_check (_env_ptr env) pCount >>= \ rc ->
    if (0 == rc) then fromIntegral <$> _peekCInt pCount else
    _throwLMDBErrNum "mdb_reader_check" rc

-- | Dump entries from reader lock table (for human consumption)
mdb_reader_list :: MDB_env -> IO String
mdb_reader_list env =
    newIORef [] >>= \ rf ->
    let onMsg cs _ =
            peekCString cs >>= \ msg ->
            modifyIORef rf (msg:) >>
            return 0
    in
    withMsgFunc onMsg $ \ pMsgFunc ->
    FFI.mdb_reader_list (_env_ptr env) pMsgFunc nullPtr >>= \ rc ->
    let toMsg = L.foldl (flip (++)) [] in
    if (0 == rc) then toMsg <$> readIORef rf else
    _throwLMDBErrNum "mdb_reader_list" rc

withMsgFunc :: FFI.MDB_msg_func -> (FunPtr FFI.MDB_msg_func -> IO a) -> IO a
withMsgFunc f = bracket (FFI.wrapMsgFunc f) freeHaskellFunPtr

-- | Begin a new transaction, possibly read-only, with a possible parent.
--
--     mdb_txn_begin env parent bReadOnly
--
-- NOTE: Unless your MDB_env was created with MDB_NOLOCK, it is necessary
-- that read-write transactions be created and completed in one Haskell
-- 'bound' thread, e.g. via forkOS or runInBoundThread. The bound threads
-- are necessary because LMDB uses OS-level mutexes which track the thread
-- ID of their owning thread.
--
-- This LMDB adapter includes its own MVar mutex to prevent more than one
-- Haskell-level thread from trying to write at the same time.
--
-- The hierarchical transactions are useful for read-write transactions.
-- They allow trying something out then aborting if it doesn't work. But
-- only one child should be active at a time, all in the same OS thread.
--
mdb_txn_begin :: MDB_env -> Maybe MDB_txn -> Bool -> IO MDB_txn
mdb_txn_begin env parent bReadOnly = mask_ $
    let bWriteTxn = not bReadOnly in
    let bLockForWrite = bWriteTxn && isNothing parent in

    -- allow only one toplevel write operation at a time.
    when bLockForWrite (_lockEnv env) >>
    let pEnv = _env_ptr env in
    let pParent = maybe nullPtr _txn_ptr parent in
    let iFlags = if bReadOnly then FFI.mdbRdonly else 0 in
    let onFailure rc =
            when bLockForWrite (_unlockEnv env) >>
            _throwLMDBErrNum "mdb_txn_begin" rc
    in
    alloca $ \ ppChildTxn ->
        FFI.mdb_txn_begin pEnv pParent iFlags ppChildTxn >>= \ rc ->
        if (0 /= rc) then onFailure rc else
        peek ppChildTxn >>= \ pChildTxn ->
        return $! MDB_txn { _txn_ptr = pChildTxn
                          , _txn_env = env
                          , _txn_rw = bWriteTxn
                          , _txn_p = parent
                          }

-- Haskell-level writer lock for the environment.
--
-- This is necessary because otherwise multiple Haskell threads on
-- the same OS thread can acquire the same lock (in windows) or
-- deadlock (in posix systems). LMDB doesn't really support M:N
-- writer threads.
--
-- Note: this will also enforce that the caller is operating in a
-- bound thread. So we can only _lockEnv from a bound thread, and
-- we can only _unlockEnv from the same thread.
_lockEnv, _unlockEnv, _canUnlockEnv :: MDB_env -> IO ()
_lockErr, _unlockErr :: LMDB_Error
_lockErr = LMDB_Error
    { e_context = "locking LMDB for write in Haskell layer"
    , e_description = "must lock from a 'bound' thread!"
    , e_code = Right MDB_PANIC
    }
_unlockErr = LMDB_Error
    { e_context = "unlock Haskell layer LMDB after write"
    , e_description = "calling thread does not own the lock!"
    , e_code = Right MDB_PANIC
    }

_lockEnv env = do
    safeThread <- isCurrentThreadBound <||> isUnlockedEnv
    unless safeThread (throwIO _lockErr)
    tid <- myThreadId
    putMVar (_env_wlock env) tid
  where
    isUnlockedEnv = hasFlag FFI.mdbNolock <$> mdb_env_get_flags_u env
    hasFlag f fs = (f == (f .&. fs))
    getA <||> getB = getA >>= \ a -> if a then return True else getB

_unlockEnv env =
    myThreadId >>= \ self ->
    let m = (_env_wlock env) in
    mask_ $
        takeMVar m >>= \ owner ->
        unless (self == owner) $
            putMVar m owner >> -- oops!
            throwIO _unlockErr

-- Check that the lock is owned by this thread.
--
-- By checking that the thread actually holds the lock
-- when aborting or committing a transaction, we avoid
-- the dangerous scenario of ending a write transaction
-- twice, which can lead to segfaults.
_canUnlockEnv env =
    myThreadId >>= \ self ->
    let m = (_env_wlock env) in
    tryReadMVar m >>= \ mowner ->
        unless (Just self == mowner) $
            throwIO _unlockErr

-- compute whether this transaction should hold the write lock.
-- If it does, unlock it. Otherwise, return.
_unlockTxn :: MDB_txn -> IO ()
_unlockTxn txn =
    let bHasLock = _txn_rw txn && isNothing (_txn_p txn) in
    when bHasLock (_unlockEnv (_txn_env txn))

_canUnlockTxn :: MDB_txn -> IO ()
_canUnlockTxn txn =
    let bHasLock = _txn_rw txn && isNothing (_txn_p txn) in
    when bHasLock (_canUnlockEnv (_txn_env txn))

-- | Access environment for a transaction.
mdb_txn_env :: MDB_txn -> MDB_env
mdb_txn_env = _txn_env

-- | Commit a transaction. Don't use the transaction after this.
mdb_txn_commit :: MDB_txn -> IO ()
mdb_txn_commit txn = mask_ $
    _canUnlockTxn txn >>
    FFI.mdb_txn_commit (_txn_ptr txn) >>= \ rc ->
    _unlockTxn txn >>
    unless (0 == rc) (_throwLMDBErrNum "mdb_txn_commit" rc)

-- | Abort a transaction. Don't use the transaction after this.
mdb_txn_abort :: MDB_txn -> IO ()
mdb_txn_abort txn = mask_ $
    _canUnlockTxn txn >>
    FFI.mdb_txn_abort (_txn_ptr txn) >>
    _unlockTxn txn

-- | Abort a read-only transaction, but don't destroy it.
-- Keep it available for mdb_txn_renew.
mdb_txn_reset :: MDB_txn -> IO ()
mdb_txn_reset txn = FFI.mdb_txn_reset (_txn_ptr txn)

-- | Renew a read-only transaction that was previously _reset.
mdb_txn_renew :: MDB_txn -> IO ()
mdb_txn_renew txn =
    FFI.mdb_txn_renew (_txn_ptr txn) >>= \ rc ->
    unless (0 == rc) (_throwLMDBErrNum "mdb_txn_renew" rc)

{-

-- I'm hoping to get a patch adding the following function into the main LMDB:
-- foreign import ccall "lmdb.h mdb_txn_id" FFI.mdb_txn_id :: MDB_txn -> IO MDB_txnid_t

-}

-- | Open a database that supports user-defined comparisons, but
-- has slightly more FFI overhead for reads and writes.
--
-- LMDB supports a small set of named databases, plus one 'main'
-- database using the null argument for the database name.
mdb_dbi_open :: MDB_txn -> Maybe String -> [MDB_DbFlag] -> IO MDB_dbi
mdb_dbi_open txn dbName flags = MDB_dbi <$> mdb_dbi_open_t txn dbName flags

-- | database statistics
mdb_stat :: MDB_txn -> MDB_dbi -> IO FFI.MDB_stat
mdb_stat txn = mdb_stat_t txn . _dbi

-- | review flags from database
mdb_dbi_flags :: MDB_txn -> MDB_dbi -> IO [MDB_DbFlag]
mdb_dbi_flags txn = mdb_dbi_flags_t txn . _dbi

-- | close the database handle.
--
-- Note: the normal use-case for LMDB is to open all the database
-- handles up front, then hold onto them until the application is
-- closed or crashed. In that case, you don't need to bother with
-- closing database handles.
mdb_dbi_close :: MDB_env -> MDB_dbi -> IO ()
mdb_dbi_close env = mdb_dbi_close_t env . _dbi

-- | remove the database and close the handle; don't use MDB_dbi
-- after this
mdb_drop :: MDB_txn -> MDB_dbi -> IO ()
mdb_drop txn = mdb_drop_t txn . _dbi

-- | clear contents of database, reset to empty
mdb_clear :: MDB_txn -> MDB_dbi -> IO ()
mdb_clear txn = mdb_clear_t txn . _dbi

mdb_dbi_open' :: MDB_txn -> Maybe String -> [MDB_DbFlag] -> IO MDB_dbi'
mdb_dbi_open' txn dbName flags = MDB_dbi' <$> mdb_dbi_open_t txn dbName flags

mdb_stat' :: MDB_txn -> MDB_dbi' -> IO FFI.MDB_stat
mdb_stat' txn = mdb_stat_t txn . _dbi'

mdb_dbi_flags' :: MDB_txn -> MDB_dbi' -> IO [MDB_DbFlag]
mdb_dbi_flags' txn = mdb_dbi_flags_t txn . _dbi'

mdb_dbi_close' :: MDB_env -> MDB_dbi' -> IO ()
mdb_dbi_close' txn = mdb_dbi_close_t txn . _dbi'

mdb_drop' :: MDB_txn -> MDB_dbi' -> IO ()
mdb_drop' txn = mdb_drop_t txn . _dbi'

mdb_clear' :: MDB_txn -> MDB_dbi' -> IO ()
mdb_clear' txn = mdb_clear_t txn . _dbi'

-- | use a nullable CString
withCStringMaybe :: Maybe String -> (CString -> IO a) -> IO a
withCStringMaybe Nothing f  = f nullPtr
withCStringMaybe (Just s) f = withCString s f

mdb_dbi_open_t :: MDB_txn -> Maybe String -> [MDB_DbFlag] -> IO FFI.MDB_dbi
mdb_dbi_open_t txn dbName flags = -- use string name
    let cdbFlags = compileDBFlags flags in
    alloca $ \ pDBI ->
    withCStringMaybe dbName $ \ cdbName ->
    FFI.mdb_dbi_open (_txn_ptr txn) cdbName cdbFlags pDBI >>= \ rc ->
    if (0 == rc) then peek pDBI else
    _throwLMDBErrNum "mdb_dbi_open" rc

mdb_stat_t :: MDB_txn -> FFI.MDB_dbi -> IO FFI.MDB_stat
mdb_stat_t txn dbi =
    alloca $ \ pStat ->
    FFI.mdb_stat (_txn_ptr txn) dbi pStat >>= \ rc ->
    if (0 == rc) then peek pStat else
    _throwLMDBErrNum "mdb_stat" rc

mdb_dbi_flags_t :: MDB_txn -> FFI.MDB_dbi -> IO [MDB_DbFlag]
mdb_dbi_flags_t txn dbi =
    alloca $ \ pFlags ->
    FFI.mdb_dbi_flags (_txn_ptr txn) dbi pFlags >>= \ rc ->
    if (0 == rc) then decompileDBFlags <$> peek pFlags else
    _throwLMDBErrNum "mdb_dbi_flags" rc

mdb_dbi_close_t :: MDB_env -> FFI.MDB_dbi -> IO ()
mdb_dbi_close_t env dbi = FFI.mdb_dbi_close (_env_ptr env) dbi

mdb_drop_t :: MDB_txn -> FFI.MDB_dbi -> IO ()
mdb_drop_t txn dbi =
    FFI.mdb_drop (_txn_ptr txn) dbi 1 >>= \ rc ->
    unless (0 == rc) (_throwLMDBErrNum "mdb_drop" rc)

mdb_clear_t :: MDB_txn -> FFI.MDB_dbi -> IO ()
mdb_clear_t txn dbi =
    FFI.mdb_drop (_txn_ptr txn) dbi 0 >>= \ rc ->
    unless (0 == rc) (_throwLMDBErrNum "mdb_clear" rc)

-- | Set a user-defined key comparison function for a database.
mdb_set_compare :: MDB_txn -> MDB_dbi -> FunPtr FFI.MDB_cmp_func -> IO ()
mdb_set_compare txn dbi fcmp =
    FFI.mdb_set_compare (_txn_ptr txn) (_dbi dbi) fcmp >>= \ rc ->
    unless (0 == rc) (_throwLMDBErrNum "mdb_set_compare" rc)

-- | Set a user-defined data comparison operator for MDB_DUPSORT databases.
mdb_set_dupsort :: MDB_txn -> MDB_dbi -> FunPtr FFI.MDB_cmp_func -> IO ()
mdb_set_dupsort txn dbi fcmp =
    FFI.mdb_set_dupsort (_txn_ptr txn) (_dbi dbi) fcmp >>= \ rc ->
    unless (0 == rc) (_throwLMDBErrNum "mdb_set_dupsort" rc)

-- zero datum
zed :: FFI.MDB_val
zed = FFI.MDB_val 0 nullPtr

-- | Access a value by key. Returns Nothing if the key is not found.
mdb_get :: MDB_txn -> MDB_dbi -> FFI.MDB_val -> IO (Maybe FFI.MDB_val)
mdb_get txn dbi key =
    withKVPtrs key zed $ \ pKey pVal ->
    FFI.mdb_get (_txn_ptr txn) (_dbi dbi) pKey pVal >>= \ rc ->
    r_get rc pVal
{-# INLINE mdb_get #-}

r_get :: CInt -> Ptr FFI.MDB_val -> IO (Maybe FFI.MDB_val)
r_get rc pVal =
    if (0 == rc) then Just <$> peek pVal else
    if (FFI.mdbNotfound == rc) then return Nothing else
    _throwLMDBErrNum "mdb_get" rc
{-# INLINE r_get #-}

-- | utility function: prepare pointers suitable for mdb_cursor_get.
withKVPtrs :: FFI.MDB_val -> FFI.MDB_val -> (Ptr FFI.MDB_val -> Ptr FFI.MDB_val -> IO a) -> IO a
withKVPtrs k v fn =
    allocaBytes (2 * sizeOf k) $ \ pK ->
    let pV = pK `plusPtr` sizeOf k in
    do poke pK k
       poke pV v
       fn pK pV
{-# INLINE withKVPtrs #-}

-- | variation on withKVPtrs with nullable value.
withKVOptPtrs :: FFI.MDB_val -> Maybe FFI.MDB_val -> (Ptr FFI.MDB_val -> Ptr FFI.MDB_val -> IO a) -> IO a
withKVOptPtrs k (Just v) fn = withKVPtrs k v fn
withKVOptPtrs k Nothing fn  = alloca $ \ pK -> poke pK k >> fn pK nullPtr


-- | Add a (key,value) pair to the database.
--
-- Returns False on MDB_KEYEXIST, and True on MDB_SUCCESS. Any other
-- return value from LMDB results in an exception. The MDB_KEYEXIST
-- result can be returned only if certain write flags are enabled.
mdb_put :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> FFI.MDB_val -> FFI.MDB_val -> IO Bool
mdb_put wf txn dbi key val =
    withKVPtrs key val $ \ pKey pVal ->
    FFI.mdb_put (_txn_ptr txn) (_dbi dbi) pKey pVal (unWriteFlags wf) >>= \ rc ->
    r_put rc
{-# INLINE mdb_put #-}

r_put :: CInt -> IO Bool
r_put rc =
    if (0 == rc) then return True else
    if (FFI.mdbKeyexist == rc) then return False else
    _throwLMDBErrNum "mdb_put" rc
{-# INLINE r_put #-}

-- | Allocate space for data under a given key. This space must be
-- filled before the write transaction commits. The idea here is to
-- avoid an extra allocation.
--
--     mdb_reserve flags txn dbi key byteCount
--
-- Note: not safe to use with MDB_DUPSORT.
-- Note: MDB_KEYEXIST will result in an exception here.
mdb_reserve :: MDB_WriteFlags -> MDB_txn -> MDB_dbi -> FFI.MDB_val -> Int -> IO FFI.MDB_val
mdb_reserve wf txn dbi key szBytes =
    withKVPtrs key (reserveData szBytes) $ \ pKey pVal ->
    FFI.mdb_put (_txn_ptr txn) (_dbi dbi) pKey pVal (unWriteFlags $ wfReserve wf) >>= \ rc ->
    if (0 == rc) then peek pVal else
    _throwLMDBErrNum "mdb_reserve" rc
{-# INLINE mdb_reserve #-}

wfReserve :: MDB_WriteFlags -> MDB_WriteFlags
wfReserve (MDB_WriteFlags wf) = MDB_WriteFlags (FFI.mdbReserve .|. wf)

reserveData :: Int -> FFI.MDB_val
reserveData szBytes = FFI.MDB_val (fromIntegral szBytes) nullPtr

-- | Delete a given key, or a specific (key,value) pair in case of
-- MDB_DUPSORT. This function will return False on a MDB_NOTFOUND
-- result, and True on MDB_SUCCESS.
--
-- Note: Ideally, LMDB would match the value even without MDB_DUPSORT.
-- But it doesn't. Under the hood, the data is replaced by a null ptr
-- if MDB_DUPSORT is not enabled (v0.9.10).
mdb_del :: MDB_txn -> MDB_dbi -> FFI.MDB_val -> Maybe FFI.MDB_val -> IO Bool
mdb_del txn dbi key mbVal =
    withKVOptPtrs key mbVal $ \ pKey pVal ->
    FFI.mdb_del (_txn_ptr txn) (_dbi dbi) pKey pVal >>= \ rc ->
    r_del rc
{-# INLINE mdb_del #-}

r_del :: CInt -> IO Bool
r_del rc =
    if (0 == rc) then return True else
    if (FFI.mdbNotfound == rc) then return False else
    _throwLMDBErrNum "mdb_del" rc
{-# INLINE r_del #-}

mdb_get' :: MDB_txn -> MDB_dbi' -> FFI.MDB_val -> IO (Maybe FFI.MDB_val)
mdb_get' txn dbi key =
    withKVPtrs key zed $ \ pKey pVal ->
    FFI.mdb_get' (_txn_ptr txn) (_dbi' dbi) pKey pVal >>= \ rc ->
    r_get rc pVal
{-# INLINE mdb_get' #-}

mdb_put' :: MDB_WriteFlags -> MDB_txn -> MDB_dbi' -> FFI.MDB_val -> FFI.MDB_val -> IO Bool
mdb_put' wf txn dbi key val =
    withKVPtrs key val $ \ pKey pVal ->
    FFI.mdb_put' (_txn_ptr txn) (_dbi' dbi) pKey pVal (unWriteFlags wf) >>= \ rc ->
    r_put rc
{-# INLINE mdb_put' #-}

mdb_reserve' :: MDB_WriteFlags -> MDB_txn -> MDB_dbi' -> FFI.MDB_val -> Int -> IO FFI.MDB_val
mdb_reserve' wf txn dbi key szBytes =
    withKVPtrs key (reserveData szBytes) $ \ pKey pVal ->
    FFI.mdb_put' (_txn_ptr txn) (_dbi' dbi) pKey pVal (unWriteFlags $ wfReserve wf) >>= \ rc ->
    if (0 == rc) then peek pVal else
    _throwLMDBErrNum "mdb_reserve" rc
{-# INLINE mdb_reserve' #-}

mdb_del' :: MDB_txn -> MDB_dbi' -> FFI.MDB_val -> Maybe FFI.MDB_val -> IO Bool
mdb_del' txn dbi key mbVal =
    withKVOptPtrs key mbVal $ \ pKey pVal ->
    FFI.mdb_del' (_txn_ptr txn) (_dbi' dbi) pKey pVal >>= \rc ->
    r_del rc
{-# INLINE mdb_del' #-}

-- | compare two values as keys in a database
mdb_cmp :: MDB_txn -> MDB_dbi -> FFI.MDB_val -> FFI.MDB_val -> IO Ordering
mdb_cmp txn dbi a b =
    withKVPtrs a b $ \ pA pB ->
    FFI.mdb_cmp (_txn_ptr txn) (_dbi dbi) pA pB >>= \ rc ->
    return (compare rc 0)
{-# INLINE mdb_cmp #-}

-- | compare two values as data in an MDB_DUPSORT database
mdb_dcmp :: MDB_txn -> MDB_dbi -> FFI.MDB_val -> FFI.MDB_val -> IO Ordering
mdb_dcmp txn dbi a b =
    withKVPtrs a b $ \ pA pB ->
    FFI.mdb_dcmp (_txn_ptr txn) (_dbi dbi) pA pB >>= \ rc ->
    return (compare rc 0)
{-# INLINE mdb_dcmp #-}

mdb_cmp' :: MDB_txn -> MDB_dbi' -> FFI.MDB_val -> FFI.MDB_val -> IO Ordering
mdb_cmp' txn dbi a b =
    withKVPtrs a b $ \ pA pB ->
    FFI.mdb_cmp' (_txn_ptr txn) (_dbi' dbi) pA pB >>= \ rc ->
    return (compare rc 0)
{-# INLINE mdb_cmp' #-}

mdb_dcmp' :: MDB_txn -> MDB_dbi' -> FFI.MDB_val -> FFI.MDB_val -> IO Ordering
mdb_dcmp' txn dbi a b =
    withKVPtrs a b $ \ pA pB ->
    FFI.mdb_dcmp' (_txn_ptr txn) (_dbi' dbi) pA pB >>= \ rc ->
    return (compare rc 0)
{-# INLINE mdb_dcmp' #-}

-- | open a cursor for the database.
mdb_cursor_open :: MDB_txn -> MDB_dbi -> IO MDB_cursor
mdb_cursor_open txn dbi =
    alloca $ \ ppCursor ->
    FFI.mdb_cursor_open (_txn_ptr txn) (_dbi dbi) ppCursor >>= \ rc ->
    if (0 /= rc) then _throwLMDBErrNum "mdb_cursor_open" rc else
    peek ppCursor >>= \ pCursor ->
    return $! MDB_cursor
        { _crs_ptr = pCursor
        , _crs_dbi = dbi
        , _crs_txn = txn
        }

mdb_cursor_open' :: MDB_txn -> MDB_dbi' -> IO MDB_cursor'
mdb_cursor_open' txn dbi =
    alloca $ \ ppCursor ->
    FFI.mdb_cursor_open' (_txn_ptr txn) (_dbi' dbi) ppCursor >>= \ rc ->
    if (0 /= rc) then _throwLMDBErrNum "mdb_cursor_open" rc else
    peek ppCursor >>= \ pCursor ->
    return $! MDB_cursor'
        { _crs_ptr' = pCursor
        , _crs_dbi' = dbi
        , _crs_txn' = txn
        }

-- | Low-level mdb_cursor_get operation, with direct control of how
-- pointers to values are allocated, whether an argument is a nullPtr,
-- and so on.
--
-- In this case, False is returned for MDB_NOTFOUND (in which case the
-- cursor should not be moved), and True is returned for MDB_SUCCESS.
-- Any other return value from LMDB will result in an exception.
--
-- Depending on the MDB_cursor_op, additional values may be returned
-- via the pointers. At the moment
mdb_cursor_get :: MDB_cursor_op -> MDB_cursor -> Ptr FFI.MDB_val -> Ptr FFI.MDB_val -> IO Bool
mdb_cursor_get op crs pKey pData = FFI.mdb_cursor_get (_crs_ptr crs) pKey pData (cursorOp op) >>= r_cursor_get
{-# INLINE mdb_cursor_get #-}

r_cursor_get :: CInt -> IO Bool
r_cursor_get rc =
    if(0 == rc) then return True else
    if(FFI.mdbNotfound == rc) then return False else
    _throwLMDBErrNum "mdb_cursor_get" rc
{-# INLINE r_cursor_get #-}

mdb_cursor_get' :: MDB_cursor_op -> MDB_cursor' -> Ptr FFI.MDB_val -> Ptr FFI.MDB_val -> IO Bool
mdb_cursor_get' op crs pKey pData = FFI.mdb_cursor_get' (_crs_ptr' crs) pKey pData (cursorOp op) >>= r_cursor_get
{-# INLINE mdb_cursor_get' #-}

-- | Low-level 'mdb_cursor_put' operation.
--
-- As with mdb_put, this returns True on MDB_SUCCESS and False for MDB_KEYEXIST,
-- and otherwise throws an exception.
--
-- Note: This function is like 'mdb_cursor_put_ptr', but creates temporary
-- pointers for keys and values.
mdb_cursor_put :: MDB_WriteFlags -> MDB_cursor -> FFI.MDB_val -> FFI.MDB_val -> IO Bool
mdb_cursor_put wf crs key val = withKVPtrs key val $ \pKey pVal -> mdb_cursor_put_ptr wf crs pKey pVal
{-# INLINE mdb_cursor_put #-}

r_cursor_put :: CInt -> IO Bool
r_cursor_put rc =
    if(0 == rc) then return True else
    if(FFI.mdbKeyexist == rc) then return False else
    _throwLMDBErrNum "mdb_cursor_put" rc
{-# INLINE r_cursor_put #-}

mdb_cursor_put' :: MDB_WriteFlags -> MDB_cursor' -> FFI.MDB_val -> FFI.MDB_val -> IO Bool
mdb_cursor_put' wf crs key val = withKVPtrs key val $ \ pKey pVal -> mdb_cursor_put_ptr' wf crs pKey pVal
{-# INLINE mdb_cursor_put' #-}

-- | Low-level 'mdb_cursor_put' operation, with direct control over pointers to
-- keys and values.
mdb_cursor_put_ptr :: MDB_WriteFlags -> MDB_cursor -> Ptr FFI.MDB_val -> Ptr FFI.MDB_val -> IO Bool
mdb_cursor_put_ptr wf crs pKey pVal = FFI.mdb_cursor_put (_crs_ptr crs) pKey pVal (unWriteFlags wf) >>= r_cursor_put
{-# INLINE mdb_cursor_put_ptr #-}

mdb_cursor_put_ptr' :: MDB_WriteFlags -> MDB_cursor' -> Ptr FFI.MDB_val -> Ptr FFI.MDB_val -> IO Bool
mdb_cursor_put_ptr' wf crs pKey pVal = FFI.mdb_cursor_put' (_crs_ptr' crs) pKey pVal (unWriteFlags wf) >>= r_cursor_put
{-# INLINE mdb_cursor_put_ptr' #-}

-- | Delete the value at the cursor.
mdb_cursor_del :: MDB_WriteFlags -> MDB_cursor -> IO ()
mdb_cursor_del wf crs = FFI.mdb_cursor_del (_crs_ptr crs) (unWriteFlags wf) >>= r_cursor_del
{-# INLINE mdb_cursor_del #-}

r_cursor_del :: CInt -> IO ()
r_cursor_del rc = unless (0 == rc) (_throwLMDBErrNum "mdb_cursor_del" rc)
{-# INLINE r_cursor_del #-}

mdb_cursor_del' :: MDB_WriteFlags -> MDB_cursor' -> IO ()
mdb_cursor_del' wf crs = FFI.mdb_cursor_del' (_crs_ptr' crs) (unWriteFlags wf) >>= r_cursor_del
{-# INLINE mdb_cursor_del' #-}

-- | Close a cursor. don't use after this. In general, cursors should
-- be closed before their associated transaction is commited or aborted.
mdb_cursor_close :: MDB_cursor -> IO ()
mdb_cursor_close crs = FFI.mdb_cursor_close (_crs_ptr crs)

mdb_cursor_close' :: MDB_cursor' -> IO ()
mdb_cursor_close' crs = FFI.mdb_cursor_close' (_crs_ptr' crs)

-- | Access transaction associated with a cursor.
mdb_cursor_txn :: MDB_cursor -> MDB_txn
mdb_cursor_txn = _crs_txn

mdb_cursor_txn' :: MDB_cursor' -> MDB_txn
mdb_cursor_txn' = _crs_txn'

-- | Access the database associated with a cursor.
mdb_cursor_dbi :: MDB_cursor -> MDB_dbi
mdb_cursor_dbi = _crs_dbi

mdb_cursor_dbi' :: MDB_cursor' -> MDB_dbi'
mdb_cursor_dbi' = _crs_dbi'

-- | count number of duplicate data items at cursor's current location.
mdb_cursor_count :: MDB_cursor -> IO Int
mdb_cursor_count crs =
    alloca $ \ pCount ->
    FFI.mdb_cursor_count (_crs_ptr crs) pCount >>= \ rc ->
    if (0 == rc) then fromIntegral <$> _peekSize pCount else
    _throwLMDBErrNum "mdb_cursor_count" rc
{-# INLINE mdb_cursor_count #-}

_peekSize :: Ptr CSize -> IO CSize
_peekSize = peek

mdb_cursor_count' :: MDB_cursor' -> IO Int
mdb_cursor_count' crs =
    alloca $ \ pCount ->
    FFI.mdb_cursor_count' (_crs_ptr' crs) pCount >>= \ rc ->
    if (0 == rc) then fromIntegral <$> _peekSize pCount else
    _throwLMDBErrNum "mdb_cursor_count" rc
{-# INLINE mdb_cursor_count' #-}


-- for cursor get...
--  I'm not really sure what I want to do here, not quite yet.
--  maybe I should write a bunch of individual functions?

{-
foreign import ccall safe "lmdb.h mdb_cursor_get" FFI.mdb_cursor_get :: Ptr MDB_cursor -> Ptr FFI.MDB_val -> Ptr FFI.MDB_val -> (#type MDB_cursor_op) -> IO CInt
-}

{-
cmpBytesToCmpFn :: (ByteString -> ByteString -> Ord) -> CmpFn
cmpBytesToCmpFn cmp vL vR = do
    lBytes <- valToVolatileByteString vL
    rBytes <- valToVolatileByteString vR
    return $! case cmp lBytes rBytes of
        LT -> -1
        EQ -> 0
        GT -> 1

-- | Create a user-defined comparison funcion over ByteStrings
wrapCmpBytes :: (ByteString -> ByteString -> Ord) -> MDB_cmp_func
wrapCmpBytes = asFFI.mdb_cmp_func . cmpBytesToCmpFn

-- | Convert a value to a bytestring in O(1) time. Note, however,
-- that this bytestring refers into a memory-mapped page in the
-- database, which may be reused after the transaction that obtained
-- the value is dropped. Developers must be careful to ensure the
-- bytestring doesn't stick around in any lazy computations.
--
-- Consider use of the safer, higher level API that will strongly
-- associate a value with a particular transaction.
valToBytes :: FFI.MDB_val -> IO ByteString
valToBytes (FFI.MDB_val sz pd) = do
    fpd <- newForeignPtr_ pd
    return $! B.fromForeignPtr fpd 0 (fromIntegral sz)

-}
