{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NondecreasingIndentation #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Directory.PosixPath
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- PosixPath based POSIX directory support
--
-----------------------------------------------------------------------------

#include "HsUnix.h"

-- hack copied from System.Posix.Files
#if !defined(PATH_MAX)
# define PATH_MAX 4096
#endif

module System.Posix.Directory.PosixPath (
   -- * Creating and removing directories
   createDirectory, removeDirectory,

   -- * Reading directories
   Common.DirStream,
   openDirStream,
   readDirStream,
   readDirStream2, readDirStream3, readDirStream4, readDirStream5,
   PosixFile(PosixFile), FileType(..),
   readDirStreamMaybe,
   Common.rewindDirStream,
   Common.closeDirStream,
   Common.DirStreamOffset,
#ifdef HAVE_TELLDIR
   Common.tellDirStream,
#endif
#ifdef HAVE_SEEKDIR
   Common.seekDirStream,
#endif

   -- * The working directory
   getWorkingDirectory,
   changeWorkingDirectory,
   Common.changeWorkingDirectoryFd,
  ) where

import Control.Monad ((>=>))
import Data.Maybe
import System.Posix.Types
import Foreign
import Foreign.C

import System.OsPath.Posix
import qualified System.Posix.Directory.Common as Common
import System.Posix.PosixPath.FilePath


-- new
import Data.ByteString.Short
import Data.Coerce
import System.OsString.Internal.Types


-- | @createDirectory dir mode@ calls @mkdir@ to
--   create a new directory, @dir@, with permissions based on
--   @mode@.
createDirectory :: PosixPath -> FileMode -> IO ()
createDirectory name mode =
  withFilePath name $ \s ->
    throwErrnoPathIfMinus1Retry_ "createDirectory" name (c_mkdir s mode)
    -- POSIX doesn't allow mkdir() to return EINTR, but it does on
    -- OS X (#5184), so we need the Retry variant here.

foreign import ccall unsafe "mkdir"
  c_mkdir :: CString -> CMode -> IO CInt

-- | @openDirStream dir@ calls @opendir@ to obtain a
--   directory stream for @dir@.
openDirStream :: PosixPath -> IO Common.DirStream
openDirStream name =
  withFilePath name $ \s -> do
    dirp <- throwErrnoPathIfNullRetry "openDirStream" name $ c_opendir s
    return (Common.DirStream dirp)

foreign import capi unsafe "HsUnix.h opendir"
   c_opendir :: CString  -> IO (Ptr Common.CDir)

-- | @readDirStream dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@, and returns the @d_name@ member of that
--   structure.
--
--   Note that this function returns an empty filepath if the end of the
--   directory stream is reached. For a safer alternative use
--   'readDirStreamMaybe'.
readDirStream :: Common.DirStream -> IO PosixPath
readDirStream = fmap (fromMaybe mempty) . readDirStreamMaybe

-- | @readDirStreamMaybe dp@ calls @readdir@ to obtain the
--   next directory entry (@struct dirent@) for the open directory
--   stream @dp@. It returns the @d_name@ member of that
--   structure wrapped in a @Just d_name@ if an entry was read and @Nothing@ if
--   the end of the directory stream was reached.
readDirStreamMaybe :: Common.DirStream -> IO (Maybe PosixPath)
readDirStreamMaybe = Common.readDirStreamWith
  (Common.dirEntName >=> peekFilePath)

d_type_FileType :: CUChar -> FileType
d_type_FileType cuchar = case cuchar of
  4 -> Dir
  8 -> Reg
  0 -> Unknown
  1 -> Fifo
  2 -> Chr
  6 -> Blk
  10 -> Lnk
  12 -> Sock
  14 -> Wht
  _ -> error "impossible"

data FileType = Unknown | Fifo | Chr | Dir | Blk | Reg | Lnk | Sock | Wht
  deriving (Eq, Ord, Show, Enum)

data PosixFile = PosixFile !PosixPath {-# UNPACK #-} !FileType
  deriving Show

readDirStream2 :: DirStream -> IO (Maybe PosixFile)
readDirStream2 (Common.DirStream dirp) = alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop :: Ptr (Ptr Common.CDirent) -> IO (Maybe PosixFile)
  loop ptr_dEnt = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if (r == 0)
         then do dEnt <- peek ptr_dEnt
                 if (dEnt == nullPtr)
                    then return Nothing
                    else do
                     entry <- (d_name dEnt >>= peekFilePath)
                     type_ <- d_type_FileType <$> d_type dEnt
                     c_freeDirEnt dEnt
                     return (Just (PosixFile entry type_))
         else do errno <- getErrno
                 if (errno == eINTR) then loop ptr_dEnt else do
                 let (Errno eo) = errno
                 if (eo == 0)
                    then return Nothing
                    else throwErrno "readDirStream"

instance Storable PosixFile where
  sizeOf _ = #{size struct dirent}
  alignment _ = #{alignment struct dirent}
  peek ptr = PosixFile
    <$> peekFilePath (plusPtr ptr #{offset struct dirent, d_name})
    <*> (d_type_FileType <$> #{peek struct dirent, d_type} ptr)
  poke _ = error "Storable PosixFile undefined"

readDirStream3 :: DirStream -> IO (Maybe PosixFile)
readDirStream3 (Common.DirStream dirp) = alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop :: Ptr (Ptr Common.CDirent) -> IO (Maybe PosixFile)
  loop ptr_dEnt = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if (r == 0)
         then do dEnt <- peek ptr_dEnt
                 if (dEnt == nullPtr)
                    then return Nothing
                    else do
                     entry <- peek (castPtr dEnt :: Ptr PosixFile)
                     c_freeDirEnt dEnt
                     return (Just entry)
         else do errno <- getErrno
                 if (errno == eINTR) then loop ptr_dEnt else do
                 let (Errno eo) = errno
                 if (eo == 0)
                    then return Nothing
                    else throwErrno "readDirStream"

readDirStream4 :: DirStream -> IO [PosixFile]
readDirStream4 (Common.DirStream dirp) = alloca $ \ptr_dEnt  -> loop ptr_dEnt id
  where
  loop :: Ptr (Ptr Common.CDirent) -> ([PosixFile] -> [PosixFile]) -> IO [PosixFile]
  loop ptr_dEnt acc = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if (r == 0)
      then do
        dEnt <- peek ptr_dEnt
        if (dEnt == nullPtr)
          then return (acc [])
          else do
            entry <- peek (castPtr dEnt :: Ptr PosixFile)
            -- c_freeDirEnt dEnt -- this required?
            loop ptr_dEnt (acc . (entry :))
      else do
        errno <- getErrno
        if (errno == eINTR)
          then loop ptr_dEnt acc
          else case errno of
            Errno eo -> if (eo == 0)
              then return (acc [])
              else throwErrno "readDirStream4"

readDirStream5 :: DirStream -> IO [PosixFile]
readDirStream5 (Common.DirStream dirp) = loop id
  where
  loop :: ([PosixFile] -> [PosixFile]) -> IO [PosixFile]
  loop acc = do
    resetErrno
    dEnt <- c_readdir_direct dirp
    if (dEnt == nullPtr)
      then return (acc [])
      else do
        entry <- peek dEnt
        loop (acc . (entry :))

-- traversing directories
foreign import ccall unsafe "__hscore_readdir"
  c_readdir :: Ptr Common.CDir -> Ptr (Ptr Common.CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr Common.CDirent -> IO ()

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr Common.CDirent -> IO CString

-- new
foreign import ccall unsafe "__hscore_d_type"
  d_type :: Ptr Common.CDirent -> IO CUChar

foreign import ccall unsafe "readdir"
  c_readdir_direct :: Ptr Common.CDir -> IO (Ptr PosixFile)

-- | @getWorkingDirectory@ calls @getcwd@ to obtain the name
--   of the current working directory.
getWorkingDirectory :: IO PosixPath
getWorkingDirectory = go (#const PATH_MAX)
  where
    go bytes = do
        r <- allocaBytes bytes $ \buf -> do
            buf' <- c_getcwd buf (fromIntegral bytes)
            if buf' /= nullPtr
                then do s <- peekFilePath buf
                        return (Just s)
                else do errno <- getErrno
                        if errno == eRANGE
                            -- we use Nothing to indicate that we should
                            -- try again with a bigger buffer
                            then return Nothing
                            else throwErrno "getWorkingDirectory"
        maybe (go (2 * bytes)) return r

foreign import ccall unsafe "getcwd"
   c_getcwd   :: Ptr CChar -> CSize -> IO (Ptr CChar)

-- | @changeWorkingDirectory dir@ calls @chdir@ to change
--   the current working directory to @dir@.
changeWorkingDirectory :: PosixPath -> IO ()
changeWorkingDirectory path =
  withFilePath path $ \s ->
     throwErrnoPathIfMinus1Retry_ "changeWorkingDirectory" path (c_chdir s)

foreign import ccall unsafe "chdir"
   c_chdir :: CString -> IO CInt

removeDirectory :: PosixPath -> IO ()
removeDirectory path =
  withFilePath path $ \s ->
     throwErrnoPathIfMinus1Retry_ "removeDirectory" path (c_rmdir s)

foreign import ccall unsafe "rmdir"
   c_rmdir :: CString -> IO CInt
